package main

import (
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2"
)

// Playlists gets the user's current playlists
func Playlists(client spotify.Client) cachedPlaylists {
	limit := 50
	playlists, err := client.CurrentUsersPlaylistsOpt(&spotify.Options{
		Limit: &limit,
	})
	if err != nil {
		// TODO: Fix "The access token expired" errors
		return cachedPlaylists{}
	}

	cached := make([]cachedPlaylistsItem, 0)

	for _, playlist := range playlists.Playlists {
		go cachePlaylist(client, playlist)

		cacheItem := cachedPlaylistsItem{
			ID:          playlist.ID.String(),
			Name:        playlist.Name,
			Owner:       playlist.Owner.ID,
			Endpoint:    playlist.ExternalURLs["spotify"],
			TotalTracks: playlist.Tracks.Total,
		}

		cached = append(cached, cacheItem)
	}

	return cachedPlaylists{
		Playlists: cached,
	}
}

// ClientFromSession gets a Spotify client from the session's user
func ClientFromSession(c *gin.Context) (*spotify.Client, error) {
	user := CurrentUser(c)
	newClient := auth.NewClient(&oauth2.Token{
		AccessToken:  user.AccessToken,
		RefreshToken: user.RefreshToken,
	})

	token, err := newClient.Token()
	if err != nil {
		return nil, err
	}

	if token.AccessToken != user.AccessToken {
		user.AccessToken = token.AccessToken
		user.RefreshToken = token.RefreshToken
		user.TokenExpiryDate = token.Expiry
		user.UpdateGP()
	}

	return &newClient, nil
}
