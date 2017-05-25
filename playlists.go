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

    playlistsEntry, err := partyCache.GetOrDefer("playlists:"+currentUser.Username, func() cache.Entry {
      playlists := Playlists(*spotifyClient)
      return cache.Forever(playlists)
    })
    if err != nil {
      c.Error(errInternal.CausedBy(err))
      c.Abort()
      return
    }

    playlists := (*playlistsEntry.Value).(models.Playlists).Playlists
		for _, playlist := range playlists {
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

func cachePlaylist(client spotify.Client, playlist spotify.SimplePlaylist) {
	tracksPage, err := client.GetPlaylistTracks(playlist.Owner.ID, playlist.ID)
  // TODO: Page through all tracks
  if err != nil {
    return
  }

  tracks := models.NewTracks(tracksPage.Tracks)
  go func() {
    var trackList models.TrackList
    trackList.SpotifyPlaylistID = null.StringFrom(playlist.ID.String())
    trackList.Data.Marshal(&tracks)
    trackList.Upsert(db, true, []string{"spotify_playlist_id"}, []string{"data"})
  }()
  partyCache.Set("playlist:"+playlist.ID.String(), cache.Forever(
    models.CachedPlaylist{
      Playlist: models.NewPlaylist(playlist),
      Tracks: tracks,
    },
  ))
}
