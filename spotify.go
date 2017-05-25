package main

import (
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2"
  "github.com/chances/chances-party/models"
)

// Playlists gets the user's current playlists
func Playlists(client spotify.Client) models.Playlists {
	limit := 50
	playlistPage, err := client.CurrentUsersPlaylistsOpt(&spotify.Options{
		Limit: &limit,
	})
	if err != nil {
		// TODO: Fix "The access token expired" errors
		return models.Playlists{}
	}

	playlists := make([]models.Playlist, len(playlistPage.Playlists))

	for i, playlist := range playlistPage.Playlists {
		go cachePlaylist(client, playlist)
    playlists[i] = models.NewPlaylist(playlist)
	}

	return models.Playlists{
		Playlists: playlists,
	}
}

// DefaultClient gets a Spotify client from the default token
// The default token is provided via
func DefaultClient() (*spotify.Client, error) {
  token, err := defaultToken()
  if err != nil {
    return nil, err
  }

  newClient := auth.NewClient(token)
  return &newClient, nil
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
