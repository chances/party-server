package main

import (
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2"
)

// Playlists gets the user's current playlists
func Playlists(client spotify.Client) []spotify.SimplePlaylist {
	limit := 50
	playlists, err := client.CurrentUsersPlaylistsOpt(&spotify.Options{
		Limit: &limit,
	})
	if err != nil {
		return nil
	}

	return playlists.Playlists
}

// ClientFromSession gets a Spotify client from the session's user
func ClientFromSession(c *gin.Context) *spotify.Client {
	user := CurrentUser(c)
  newClient := auth.NewClient(&oauth2.Token{
		AccessToken:  user.AccessToken,
		RefreshToken: user.RefreshToken,
	})

  token, err := newClient.Token()
  if err != nil {
    c.Error(errInternal.CausedBy(err))
    return nil
  }

  if token.AccessToken != user.AccessToken {
    user.AccessToken = token.AccessToken
    user.RefreshToken = token.RefreshToken
    user.TokenExpiryDate = token.Expiry
    user.UpdateGP()
  }

  return &newClient
}
