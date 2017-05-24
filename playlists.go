package main

import (
	"net/http"

	"github.com/chances/chances-party/cache"
	"github.com/chances/chances-party/models"
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
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
		for _, playlist := range playlists.Playlists {
			if id == playlist.ID {
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

// TODO: Heavily refactor this playlist caching crap (Add playlist model)
type cachedPlaylists struct {
	Playlists []cachedPlaylistsItem
}

type cachedPlaylistsItem struct {
	ID          string `json:"id"`
	Name        string `json:"name"`
	Owner       string `json:"owner"`
	Endpoint    string `json:"endpoint"`
	TotalTracks uint   `json:"total_tracks"`
}

type cachedPlaylist struct {
	Playlist spotify.SimplePlaylist
	Tracks   []spotify.PlaylistTrack
}

func cachePlaylist(client spotify.Client, playlist spotify.SimplePlaylist) {
	tracks, err := client.GetPlaylistTracks(playlist.Owner.ID, playlist.ID)
	if err == nil {
		partyCache.Set("playlist:"+playlist.ID.String(), cache.Forever(
			cachedPlaylist{
				Playlist: playlist,
				Tracks:   tracks.Tracks,
			},
		))
	}
}
