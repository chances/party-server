package main

import (
	"encoding/json"
	"net/http"
	"strings"
	"time"

	"github.com/chances/chances-party/models"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
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

// AuthRequired guards against unauthenticated sessions
func AuthRequired() gin.HandlerFunc {
	return func(c *gin.Context) {
		if !IsLoggedIn(c) {
			c.Error(errUnauthorized)
			c.Abort()
			return
		}

		c.Next()
	}
}

func login(c *gin.Context) {
	state := uuid.NewV4().String()

	session := DefaultSession(c)
	err := session.Set("AUTH_STATE", state)
	if err != nil {
		c.Error(errAuth.WithDetail("Couldn't save session").CausedBy(err))
		c.Abort()
		return
	}

	c.Redirect(http.StatusSeeOther, auth.AuthURL(state))
}

// IDEA: Convert error handling shenanigans to Observable chain
func spotifyCallback(c *gin.Context) {
	session := DefaultSession(c)
	sessionState, err := session.Get("AUTH_STATE")
	if err != nil {
		c.Error(errAuth.WithDetail("Could not retrieve auth state").CausedBy(err))
		c.Abort()
		return
	}
	session.Delete("AUTH_STATE")

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

	var user models.User
	user.Username = spotifyUser.ID
	user.SpotifyUser = spotifyUserJSON
	user.AccessToken = token.AccessToken
	user.RefreshToken = token.RefreshToken
	user.TokenExpiryDate = token.Expiry
	user.TokenScope = scopes
	user.UpdatedAt = time.Now()

	err = user.Upsert(db, true, []string{"username"}, []string{
		"spotify_user", "access_token", "refresh_token",
		"token_expiry_date", "token_scope", "updated_at",
	})
	if err != nil {
		c.Error(errAuth.WithDetail("Could not create/update user").CausedBy(err))
		c.Abort()
		return
	}

	c.Set("user", user)
	session.Set("USER", user.Username)

	// Successfully logged in
	c.Redirect(http.StatusSeeOther, "/")
}
