package main

import (
	"encoding/json"
	"net/http"

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

// TODO: Heavily refactor this playlist caching crap (Add playlist model)
type cachedPlaylists struct {
	Playlists []cachedPlaylistsItem
}

type cachedPlaylistsItem struct {
	ID          string
	Name        string
	TotalTracks uint
}

func cachePlaylists(key string, client spotify.Client, playlists []spotify.SimplePlaylist) {
	cache := make([]cachedPlaylistsItem, 0)

	for _, playlist := range playlists {
		go cachePlaylist(client, playlist)

		cacheItem := cachedPlaylistsItem{
			ID:          playlist.ID.String(),
			Name:        playlist.Name,
			TotalTracks: playlist.Tracks.Total,
		}

		cache = append(cache, cacheItem)
	}

	cacheJSON, _ := json.Marshal(&cache)

	redisSet(key, string(cacheJSON))
}

type cachedPlaylist struct {
	Playlist spotify.SimplePlaylist
	Tracks   []spotify.PlaylistTrack
}

func cachePlaylist(client spotify.Client, playlist spotify.SimplePlaylist) {
	tracks, err := client.GetPlaylistTracks(playlist.Owner.ID, playlist.ID)
	if err == nil {
		cache := cachedPlaylist{
			Playlist: playlist,
			Tracks:   tracks.Tracks,
		}

		playlistJSON, _ := json.Marshal(&cache)

		redisSet("playlist:"+playlist.ID.String(), string(playlistJSON))
	}
}
