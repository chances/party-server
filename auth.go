package main

import (
	"encoding/json"
	"log"
	"net/http"
	"strings"

	"github.com/chances/chances-party/models"
	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
	"github.com/vattle/sqlboiler/queries/qm"
	"github.com/zmb3/spotify"
)

var (
	auth   spotify.Authenticator
	scopes string
)

func setupAuth() {
	auth = spotify.NewAuthenticator(
		getenvOrFatal("SPOTIFY_CALLBACK"),
		spotify.ScopeUserReadPrivate,
		spotify.ScopePlaylistReadPrivate,
		spotify.ScopePlaylistReadCollaborative,
	)
	auth.SetAuthInfo(getenvOrFatal("SPOTIFY_APP_KEY"), getenvOrFatal("SPOTIFY_APP_SECRET"))

	s := make([]string, 3)
	s[0] = spotify.ScopeUserReadPrivate
	s[1] = spotify.ScopePlaylistReadPrivate
	s[2] = spotify.ScopePlaylistReadCollaborative
	scopes = strings.Join(s, " ")
}

func login(c *gin.Context) {
	state := uuid.NewV4().String()

	session := sessions.Default(c)
	session.Set("AUTH_STATE", state)
	err := session.Save()
	if err != nil {
		c.Error(errAuth.WithDetail("Couldn't save session").CausedBy(err))
		c.Abort()
		return
	}

	c.Redirect(http.StatusSeeOther, auth.AuthURL(state))
}

// IDEA: Convert error handling shenanigans to Observable chain
func spotifyCallback(c *gin.Context) {
	session := sessions.Default(c)
	var sessionState string
	v := session.Get("AUTH_STATE")
	log.Printf("\n\n%s\n\n", v)
	if v == nil {
		c.Error(errAuth.WithDetail("Auth state is nil"))
		c.Abort()
		return
	}
	sessionState, ok := v.(string)
	log.Printf("blah\n\n%s\n\n", sessionState)
	if !ok {
		c.Error(errAuth.WithDetail("Auth state is *not* string"))
		c.Abort()
		return
	}
	session.Delete("AUTH_STATE")
	session.Save()

	// Validate OAuth state
	oauthState := c.Request.FormValue("state")
	if oauthState != sessionState {
		c.Error(errAuth.WithDetail("Auth failed, mismatched state"))
		c.Abort()
		return
	}

	// Retrieve token
	token, err := auth.Token(sessionState, c.Request)
	if err != nil {
		c.Error(errAuth.WithDetail("Token request failed").CausedBy(err))
		c.Abort()
		return
	}

	// Get logged in user
	client := auth.NewClient(token)
	spotifyUser, err := client.CurrentUser()
	if err != nil {
		c.Error(errAuth.WithDetail("Could not get user").CausedBy(err))
		c.Abort()
		return
	}

	spotifyUserJSON, err := json.Marshal(spotifyUser)
	if err != nil {
		c.Error(errAuth.WithDetail("Could not serialize user").CausedBy(err))
		c.Abort()
		return
	}

	existingUser, err := models.UsersG(qm.Where("username = ?", spotifyUser.ID)).One()
	if err == nil {
		existingUser.SpotifyUser = spotifyUserJSON
		existingUser.AccessToken = token.AccessToken
		existingUser.RefreshToken = token.RefreshToken
		existingUser.TokenExpiryDate = token.Expiry
		existingUser.TokenScope = scopes

		err := existingUser.UpdateG()
		if err != nil {
			c.Error(errAuth.WithDetail("Could not update user").CausedBy(err))
			c.Abort()
			return
		}
	} else {
		newUser := models.User{
			Username:        spotifyUser.ID,
			SpotifyUser:     spotifyUserJSON,
			AccessToken:     token.AccessToken,
			RefreshToken:    token.RefreshToken,
			TokenExpiryDate: token.Expiry,
			TokenScope:      scopes,
		}

		err := newUser.InsertG()
		if err != nil {
			c.Error(errAuth.WithDetail("Could not create user").CausedBy(err))
			c.Abort()
			return
		}
	}

	// Successfully logged in
	c.Redirect(http.StatusSeeOther, "/")
}
