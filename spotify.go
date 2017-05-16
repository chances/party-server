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
func ClientFromSession(c *gin.Context) spotify.Client {
	user := CurrentUser(c)
	return auth.NewClient(&oauth2.Token{
		AccessToken:  user.AccessToken,
		RefreshToken: user.RefreshToken,
	})
}
