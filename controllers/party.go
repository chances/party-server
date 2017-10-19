package controllers

import (
	"crypto/rand"
	"database/sql"
	"encoding/base64"
	"encoding/json"
	"fmt"
	pseudoRand "math/rand"
	"net/http"
	"time"

	null "gopkg.in/nullbio/null.v6"

	"github.com/chances/party-server/cache"
	e "github.com/chances/party-server/errors"
	"github.com/chances/party-server/events"
	"github.com/chances/party-server/models"
	"github.com/chances/party-server/session"
	s "github.com/chances/party-server/spotify"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
	"github.com/vattle/sqlboiler/queries/qm"
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

// Get the current user's party
func (cr *Party) Get() gin.HandlerFunc {
	return func(c *gin.Context) {
		currentParty, err := session.CurrentParty(c)
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.JSON(http.StatusNotFound, models.EmptyRespose)
			return
		}

		guests, err := currentParty.Guests()
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		cr.augmentAndRespondWithParty(c, currentParty, guests)
	}
}

// Join a party as a guest
func (cr *Party) Join() gin.HandlerFunc {
	return func(c *gin.Context) {
	  sesh := session.DefaultSession(c)

	  var joinParty struct {
			Data struct {
				RoomCode string `json:"room_code" binding:"required"`
			} `json:"data" binding:"required"`
		}

		if err := c.Bind(&joinParty); err != nil {
			c.Error(e.BadRequest.WithDetail("Unexpected request body").CausedBy(err))
			c.Abort()
			return
		}

		party, err := models.PartiesG(qm.Where("room_code=?", joinParty.Data.RoomCode)).One()
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.JSON(http.StatusNotFound, models.EmptyRespose)
			return
		}

		guests, err := party.Guests()
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		// If the user is fully authenticated skip guest initialization and
		//  respond with augmented party
		if session.IsLoggedIn(c) {
		  // TODO: Make sure this auth'd user started _this_ party
			cr.augmentAndRespondWithParty(c, party, guests)
			return
		}

		// TODO: Handle party has ended, respond with some error code, 404 seems wrong...

		// It is a bad request to try to join a party the guest has already joined
    // If the user is already a guest and they've joined _this_ party then
    //  respond with augmented party
    //
    // Otherwise it is a bad request if they are a guest and have NOT joined
    //  _this_ party
    if session.IsGuest(c) {
      guest := *session.CurrentGuest(c)
      if guest["Party"] == party.ID {
        cr.augmentAndRespondWithParty(c, party, guests)
        return
      } else {
        c.Error(e.BadRequest.WithDetail("Already joined a party"))
        c.Abort()
        return
      }
    }

		guests = append(guests, models.NewGuest(""))
		err = party.UpdateGuestList(guests)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		guestToken := uuid.NewV4().String()

		c.Set("guest", guestToken)
		err = sesh.Set("GUEST", guestToken)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}
		err = cr.Cache.Set(guestToken, cache.Expires(
			time.Now().Add(time.Minute*time.Duration(30)),
			gin.H{
				"Token":  guestToken,
				"Party":  party.ID,
				"Index":  len(guests) - 1,
			},
		))
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}
		// TODO: Goroutine to clean expired tokens (also removes guest from party)

		cr.augmentAndRespondWithParty(c, party, guests)
	}
}

func (cr *Party) augmentAndRespondWithParty(c *gin.Context, party *models.Party, guests []models.Guest) {
	response := publicParty{
		Location: party.Location,
		RoomCode: party.RoomCode,
		Ended:    party.Ended,
	}
	if len(guests) > 0 {
		guestsJsonRaw, _ := json.Marshal(guests)
		guestsJson := types.JSON(guestsJsonRaw)
		response.Guests = &guestsJson
	}
	if party.CurrentTrack.Valid {
		var track models.Track
		err := party.CurrentTrack.Unmarshal(&track)
		if err == nil {
			response.CurrentTrack = &track
		}
	}

	partyJson, _ := json.Marshal(response)
	events.Event(party.RoomCode + "party").Submit(string(partyJson))

	c.JSON(http.StatusOK, models.NewResponse(
		response.RoomCode, "party",
		cr.RequestURI(c),
		response,
	))
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

		c.JSON(http.StatusOK, models.NewResponse(
			party.RoomCode, "party",
			cr.RequestURI(c),
			party,
		))
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
