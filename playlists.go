package main

import (
	"net/http"

	"github.com/chances/chances-party/models"
	"github.com/gin-gonic/gin"
	null "gopkg.in/nullbio/null.v6"
)

func patchPlaylist(c *gin.Context) {
	var patchPlaylist struct {
		Data struct {
			ID string `json:"id" binding:"required"`
		} `json:"data" binding:"required"`
	}

	if c.Bind(&patchPlaylist) == nil {
		id := patchPlaylist.Data.ID
		currentUser := CurrentUser(c)

		spotifyClient, err := ClientFromSession(c)
		if err != nil {
			c.Error(errInternal.CausedBy(err))
			c.Abort()
			return
		}

		playlists := Playlists(*spotifyClient) // TODO: Cache these?
		for _, playlist := range playlists {
			if id == playlist.ID.String() {
				currentUser.SpotifyPlaylistID = null.StringFrom(id)
				err := currentUser.UpdateG("spotify_playlist_id")
				if err != nil {
					c.Error(errInternal.WithDetail("Could not update user").CausedBy(err))
					c.Abort()
					return
				}

				c.JSON(http.StatusOK, models.Response{
					Data: playlist,
				})
				return
			}
		}

		c.Error(errBadRequest.WithDetail("Invalid playlist id"))
	}
}
