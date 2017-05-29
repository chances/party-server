package controllers

import (
	"net/http"

	"github.com/chances/chances-party/cache"
	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/models"
	"github.com/chances/chances-party/session"
	s "github.com/chances/chances-party/spotify"
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
)

// Index controller
type Index struct {
	Controller
	spotifyAuth spotify.Authenticator
}

// NewIndex creates a new Index controller
func NewIndex(c cache.Store, auth spotify.Authenticator) Index {
	newIndex := Index{
		spotifyAuth: auth,
	}
	newIndex.Cache = c
	return newIndex
}

// Get the index page view
func (cr *Index) Get() gin.HandlerFunc {
	return func(c *gin.Context) {
		sesh := session.DefaultSession(c)
		flashedError, err := sesh.Error()
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
		}
		if session.IsLoggedIn(c) {
			currentUser := session.CurrentUser(c)
			var spotifyUser spotify.PrivateUser
			err := currentUser.SpotifyUser.Unmarshal(&spotifyUser)
			if err != nil {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			spotifyClient, err := s.ClientFromSession(c, cr.spotifyAuth)
			if err != nil {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			var currentPlaylist *models.Playlist
			playlists, err := s.Playlists(currentUser.Username, *spotifyClient)
			if err != nil {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			for _, playlist := range *playlists {
				if currentUser.SpotifyPlaylistID.String == playlist.ID {
					currentPlaylist = &playlist
					break
				}
			}
			c.HTML(http.StatusOK, "index.html", gin.H{
				"user":            spotifyUser,
				"currentPlaylist": currentPlaylist,
				"playlists":       playlists,
				"error":           flashedError,
			})
		} else {
			c.HTML(http.StatusOK, "index.html", gin.H{
				"error": flashedError,
			})
		}
	}
}
