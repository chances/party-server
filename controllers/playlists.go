package controllers

import (
	"encoding/gob"
	"net/http"

	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/models"
	"github.com/chances/chances-party/session"
	s "github.com/chances/chances-party/spotify"
	"github.com/gin-gonic/gin"
	"gopkg.in/nullbio/null.v6"
)

// Playlists controller
type Playlists struct {
	Controller
}

// NewPlaylists creates a new Playlists controller
func NewPlaylists() Playlists {
	gob.Register(models.CachedPlaylist{})
	gob.Register(models.Playlists{})
	gob.Register(models.Playlist{})
	gob.Register([]models.Playlist{})

	newPlaylists := Playlists{}
	newPlaylists.Setup()
	return newPlaylists
}

// Patch the current playlist via the new playlist's Spotify ID
func (cr *Playlists) Patch() gin.HandlerFunc {
	return func(c *gin.Context) {
		var patchPlaylist struct {
			Data struct {
				ID string `json:"id" binding:"required"`
			} `json:"data" binding:"required"`
		}

		if err := c.Bind(&patchPlaylist); err != nil {
			c.Error(e.BadRequest.WithDetail("Unexpected request body").CausedBy(err))
			c.Abort()
			return
		}

		id := patchPlaylist.Data.ID
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

		for _, playlist := range playlists {
			if id == playlist.ID {
				currentUser.SpotifyPlaylistID = null.StringFrom(id)
				err := currentUser.UpdateG("spotify_playlist_id")
				if err != nil {
					c.Error(e.Internal.WithDetail("Could not update user").CausedBy(err))
					c.Abort()
					return
				}

				c.JSON(http.StatusOK, models.Response{
					Data: playlist,
				})
				return
			}
		}

		c.Error(e.BadRequest.WithDetail("Invalid playlist id"))
	}
}
