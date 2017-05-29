package controllers

import (
	"crypto/rand"
	"database/sql"
	"encoding/base64"
	"fmt"
	pseudoRand "math/rand"
	"net/http"
	"time"

	null "gopkg.in/nullbio/null.v6"

	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/models"
	"github.com/chances/chances-party/session"
	s "github.com/chances/chances-party/spotify"
	"github.com/gin-gonic/gin"
	"github.com/vattle/sqlboiler/types"
)

// Party controller
type Party struct {
	Controller
}

// NewParty creates a new Party controller
func NewParty() Party {
	newParty := Party{}
	newParty.Setup()

	// Seed room code generator
	pseudoRand.Seed(time.Now().UnixNano())
	for i := 0; i < (pseudoRand.Intn(150-25) + 25); i++ {
		generateRoomCode(pseudoRand.Intn(100-5) + 5)
	}

	return newParty
}

type publicParty struct {
	Location     types.JSON    `json:"location"`
	RoomCode     string        `json:"room_code"`
	Ended        bool          `json:"ended"`
	Guests       *types.JSON   `json:"guests,omitempty"`
	CurrentTrack *models.Track `json:"current_track,omitempty"`
}

// Get the curren't user's party
func (cr *Party) Get() gin.HandlerFunc {
	return func(c *gin.Context) {
		currentUser := session.CurrentUser(c)

		currentParty, err := currentUser.PartyG().One()
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.JSON(http.StatusNotFound, models.Response{
				Data: gin.H{},
			})
		}
		partyGuests, err := currentParty.GuestG().One()
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}
		}

		response := publicParty{
			Location: currentParty.Location,
			RoomCode: currentParty.RoomCode,
			Ended:    currentParty.Ended,
		}
		if partyGuests != nil {
			response.Guests = &partyGuests.Data
		}
		if currentParty.CurrentTrack.Valid {
			var track models.Track
			err = currentParty.CurrentTrack.Unmarshal(&track)
			if err == nil {
				response.CurrentTrack = &track
			}
		}

		c.JSON(http.StatusOK, models.Response{
			Data: response,
		})
	}
}

// Start a new party for the current user
func (cr *Party) Start() gin.HandlerFunc {
	return func(c *gin.Context) {
		var newParty struct {
			Data struct {
				Host       string `json:"host" binding:"required"`
				PlaylistID string `json:"playlist_id" binding:"required"`
			} `json:"data" binding:"required"`
		}

		if err := c.Bind(&newParty); err != nil {
			c.Error(e.BadRequest.WithDetail("Unexpected request body").CausedBy(err))
			c.Abort()
			return
		}

		playlistID := newParty.Data.PlaylistID
		currentUser := session.CurrentUser(c)

		spotifyClient, err := cr.ClientFromSession(c)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		playlists, err := s.Playlists(currentUser.Username, *spotifyClient)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		var currentPlaylist *models.Playlist
		for _, playlist := range playlists {
			if playlistID == playlist.ID {
				currentPlaylist = &playlist
			}
		}

		// Playlist doesn't belong to user, bad request
		if currentPlaylist == nil {
			errMessage := fmt.Sprintf("Invalid playlist id %v. User '%s' does not own or subscribe to given playlist", playlistID, currentUser.Username)
			c.Error(e.BadRequest.WithDetail(errMessage))
		}

		location := gin.H{
			"host_name": newParty.Data.Host,
		}

		party := models.Party{
			RoomCode:     generateRoomCode(roomCodeLength),
			CurrentTrack: null.JSONFrom(nil),
		}
		party.Location.Marshal(location)

		// New party's queue
		playlist, err := s.Playlist(currentUser.Username, currentPlaylist.ID, *spotifyClient)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}
		queue := models.TrackList{
			SpotifyPlaylistID: null.NewString("", false),
		}
		queue.Data.Marshal(&playlist.Tracks)
		err = party.SetQueueG(true, &queue)
		if err != nil {
			c.Error(e.Internal.WithDetail("Could not create queue").CausedBy(err))
			c.Abort()
			return
		}

		// New party's playback history
		history := models.TrackList{
			SpotifyPlaylistID: null.NewString("", false),
		}
		history.Data.Marshal(make([]models.Track, 0))
		err = party.SetHistoryG(true, &history)
		if err != nil {
			c.Error(e.Internal.WithDetail("Could not create history").CausedBy(err))
			c.Abort()
			return
		}

		// New party's guest list
		guests := make([]models.Guest, 0)
		guestList := models.GuestList{}
		guestList.Data.Marshal(guests)
		err = party.SetGuestG(true, &guestList)
		if err != nil {
			c.Error(e.Internal.WithDetail("Could not create guest list").CausedBy(err))
			c.Abort()
			return
		}

		err = currentUser.SetPartyG(true, &party)
		if err != nil {
			c.Error(e.Internal.WithDetail("Could not create party").CausedBy(err))
			c.Abort()
			return
		}

		c.JSON(http.StatusOK, models.Response{
			Data: party,
		})
	}
}

const roomCodeLength = 3 // Generates string of length 4

func generateRoomCode(s int) string {
	b := make([]byte, s)
	_, err := rand.Read(b)
	if err != nil {
		return ""
	}
	return base64.URLEncoding.EncodeToString(b)
}
