package controllers

import (
	"database/sql"
	"encoding/json"
	"strconv"
	"time"

	e "github.com/chances/party-server/errors"
	"github.com/chances/party-server/events"
	"github.com/chances/party-server/models"
	"github.com/chances/party-server/session"
	"github.com/gin-gonic/gin"
	"github.com/vattle/sqlboiler/types"
)

// Playback controller
type Playback struct {
	Controller
}

// NewPlayback creates a new Playback controller
func NewPlayback() Playback {
	newPlayback := Playback{}
	newPlayback.Setup()

	return newPlayback
}

// Play the party's playlist or resume from paused state
func (cr *Playback) Play() gin.HandlerFunc {
	return func(c *gin.Context) {
		currentParty, err := session.CurrentParty(c)
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.Error(e.NotFound.WithMessage("Host has not started a party"))
			c.Abort()
			return
		}

		var currentTrack models.PlayingTrack
		if !currentParty.CurrentTrack.Valid {
			// Begin playing the party's queue
			shuffleParam := c.Query("shuffle")
			shuffle := shuffleParam != "" && shuffleParam != "no" && shuffleParam != "0"

			queue, err := currentParty.QueueTracks()
			if err != nil {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			// Shuffle the queue if requested
			if shuffle {
				queue = models.Shuffle(queue)
			}

			track, err := cr.popTrackAndPlay(queue, currentParty.QueueID)
			if err != nil {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}
			currentTrack = *track
		} else {
			// Resume the current track
			err = currentParty.CurrentTrack.Unmarshal(currentTrack)
			if err != nil {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			// It is a bad request to resume a track that is already playing
			if !currentTrack.Paused {
				c.Error(e.BadRequest.WithDetail("Current track is not paused"))
				c.Abort()
				return
			}

			currentTrack.Paused = false
			currentTrack.BeganPlaying = time.Now().UTC().Add(
				time.Duration(currentTrack.Elapsed) * time.Second * -1,
			)
		}

		err = cr.updateTrackAndBroadcast(currentParty, &currentTrack)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}
	}
}

// Pause the party's playlist from playing
func (cr *Playback) Pause() gin.HandlerFunc {
	return func(c *gin.Context) {
		currentParty, err := session.CurrentParty(c)
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.Error(e.NotFound.WithMessage("Host has not started a party"))
			c.Abort()
			return
		}

		// It is a bad request to pause when no track is playing
		if !currentParty.CurrentTrack.Valid {
			c.Error(e.BadRequest.WithDetail("Host is not playing music"))
			c.Abort()
			return
		}

		// Pause the current track
		var currentTrack models.PlayingTrack
		err = currentParty.CurrentTrack.Unmarshal(currentTrack)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		elapsedParam, exists := c.GetQuery("elapsed")
		if !exists {
			c.Error(e.BadRequest.WithDetail("Must provide elapsed duration to pause playback"))
			c.Abort()
			return
		}
		elapsed, err := strconv.ParseUint(elapsedParam, 10, 64)
		if err != nil {
			c.Error(
				e.BadRequest.
					WithDetail("Must provide valid elapsed duration to pause playback").
					CausedBy(err),
			)
			c.Abort()
			return
		}

		// It is a bad request to pause a track that is already paused
		if currentTrack.Paused {
			c.Error(e.BadRequest.WithDetail("Current track is already paused"))
			c.Abort()
			return
		}

		currentTrack.Paused = true
		currentTrack.Elapsed = uint(elapsed)
		err = cr.updateTrackAndBroadcast(currentParty, &currentTrack)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}
	}
}

func (cr *Playback) Skip() gin.HandlerFunc {
	return func(c *gin.Context) {
		currentParty, err := session.CurrentParty(c)
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.Error(e.NotFound.WithMessage("Host has not started a party"))
			c.Abort()
			return
		}

		// It is a bad request to skip when no track is playing
		if !currentParty.CurrentTrack.Valid {
			c.Error(e.BadRequest.WithDetail("Host is not playing music"))
			c.Abort()
			return
		}

		var currentTrack models.PlayingTrack
		err = currentParty.CurrentTrack.Unmarshal(currentTrack)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}
		lastTrack := currentTrack.Track

		// Pop next track from party's queue
		queue, err := currentParty.QueueTracks()
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		newTrack, err := cr.popTrackAndPlay(queue, currentParty.QueueID)
		if err != nil {
			c.Error(e.Internal.WithDetail("Could not pop track from queue").CausedBy(err))
			c.Abort()
			return
		}
		err = cr.updateTrackAndBroadcast(currentParty, newTrack)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		// Push last track to party's history
		history, err := currentParty.HistoryTracks()
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		err = cr.pushTrack(history, currentParty.HistoryID, &lastTrack)
		if err != nil {
			c.Error(e.Internal.WithDetail("Could not push track to history").CausedBy(err))
			c.Abort()
			return
		}
	}
}

func (cr *Playback) popTrackAndPlay(queue *[]models.Track, queueID int) (*models.PlayingTrack, error) {
	// Pop first track from top of queue
	firstTrack := (*queue)[0]
	copy(*queue, (*queue)[1:])
	updatedQueue := (*queue)[:len(*queue)-1]
	// Update the queue
	queueJsonRaw, err := json.Marshal(updatedQueue)
	if err != nil {
		return nil, err
	}
	queueJson := types.JSON(queueJsonRaw)
	queueTrackList := models.TrackList{
		ID:   queueID,
		Data: queueJson,
	}
	err = queueTrackList.UpdateG("data")
	if err != nil {
		return nil, err
	}

	// TODO: Send "queue" changed event on party's message channel

	// "Play" the track
	currentTrack := models.PlayingTrack{
		Track:   firstTrack,
		Paused:  false,
		Elapsed: 0,
	}
	currentTrack.BeganPlaying = time.Now().UTC()

	return &currentTrack, nil
}

func (cr *Playback) pushTrack(history *[]models.Track, historyID int, track *models.Track) error {
	// Push track to top of history stack
	updatedHistory := append([]models.Track{*track}, *history...)
	historyJsonRaw, err := json.Marshal(updatedHistory)
	if err != nil {
		return err
	}
	historyJson := types.JSON(historyJsonRaw)
	historyTrackList := models.TrackList{
		ID:   historyID,
		Data: historyJson,
	}
	err = historyTrackList.UpdateG("data")
	if err != nil {
		return err
	}

	// TODO: Send "history" changed event on party's message channel

	return nil
}

func (cr *Playback) updateTrackAndBroadcast(party *models.Party, track *models.PlayingTrack) error {
	err := party.CurrentTrack.Marshal(track)
	if err != nil {
		return err
	}
	err = party.UpdateG("current_track")
	if err != nil {
		return err
	}

	// Broadcast to clients that the party's current track has changed
	publicParty, err := party.Public()
	if err != nil {
		return err
	}
	go events.UpdateParty(publicParty)

	return nil
}
