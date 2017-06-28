package controllers

import (
	"encoding/gob"
	"encoding/json"
	"net/http"
	"strings"
	"time"

	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/models"
	"github.com/chances/chances-party/session"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
	"github.com/vattle/sqlboiler/boil"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/clientcredentials"
)

// Auth controller for Party authentication
type Auth struct {
	Controller
	spotifyScopes string
	jwtSigningKey []byte
}

// NewAuth creates a new Auth controller
func NewAuth(spotifyKey, spotifySecret, spotifyCallback, jwtSecret string) Auth {
	gob.Register(oauth2.Token{})

	// TODO: Refactor out "auth" Spotify client factories
	spotifyAuth := spotify.NewAuthenticator(
		spotifyCallback,
		spotify.ScopeUserReadPrivate,
		spotify.ScopePlaylistReadPrivate,
		spotify.ScopePlaylistReadCollaborative,
	)
	spotifyAuth.SetAuthInfo(spotifyKey, spotifySecret)

	SetSpotifyAuth(
		spotifyAuth,
		clientcredentials.Config{
			ClientID:     spotifyKey,
			ClientSecret: spotifySecret,
			TokenURL:     "https://accounts.spotify.com/api/token",
		},
	)

	spotifyScopes := make([]string, 3)
	spotifyScopes[0] = spotify.ScopeUserReadPrivate
	spotifyScopes[1] = spotify.ScopePlaylistReadPrivate
	spotifyScopes[2] = spotify.ScopePlaylistReadCollaborative

	newAuth := Auth{
		spotifyScopes: strings.Join(spotifyScopes, " "),
		jwtSigningKey: []byte(jwtSecret),
	}
	newAuth.Setup()
	return newAuth
}

// Login ot Party via Spotify's Authorization Grant OAuth flow
func (cr *Auth) Login() gin.HandlerFunc {
	return func(c *gin.Context) {
		state := uuid.NewV4().String()

		sesh := session.DefaultSession(c)
		err := sesh.Set("AUTH_STATE", state)
		if err != nil {
			c.Error(e.Auth.WithDetail("Couldn't save session").CausedBy(err))
			c.Abort()
			return
		}

		c.Redirect(http.StatusSeeOther, cr.SpotifyAuth.AuthURL(state))
	}
}

// SpotifyCallback completes Spotify's Authorization Grant OAuth flow
// IDEA: Convert error handling shenanigans to Observable chain
func (cr *Auth) SpotifyCallback() gin.HandlerFunc {
	return func(c *gin.Context) {
		sesh := session.DefaultSession(c)
		sessionState, err := sesh.Get("AUTH_STATE")
		if err != nil {
			c.Error(e.Auth.WithDetail("Could not retrieve auth state").CausedBy(err))
			c.Abort()
			return
		}
		sesh.Delete("AUTH_STATE")

		// Validate OAuth state
		oauthState := c.Request.FormValue("state")
		if oauthState != sessionState {
			c.Error(e.Auth.WithDetail("Auth failed, mismatched state"))
			c.Abort()
			return
		}

		// Retrieve token
		token, err := cr.SpotifyAuth.Token(sessionState, c.Request)
		if err != nil {
			c.Error(e.Auth.WithDetail("Token request failed").CausedBy(err))
			c.Abort()
			return
		}

		// Get logged in user
		client := cr.SpotifyAuth.NewClient(token)
		spotifyUser, err := client.CurrentUser()
		if err != nil {
			c.Error(e.Auth.WithDetail("Could not get user").CausedBy(err))
			c.Abort()
			return
		}

		spotifyUserJSON, err := json.Marshal(spotifyUser)
		if err != nil {
			c.Error(e.Auth.WithDetail("Could not serialize user").CausedBy(err))
			c.Abort()
			return
		}

		var user models.User
		user.Username = spotifyUser.ID
		user.SpotifyUser = spotifyUserJSON
		user.AccessToken = token.AccessToken
		user.RefreshToken = token.RefreshToken
		user.TokenExpiryDate = token.Expiry
		user.TokenScope = cr.spotifyScopes
		user.UpdatedAt = time.Now()

		err = user.Upsert(boil.GetDB(), true, []string{"username"}, []string{
			"spotify_user", "access_token", "refresh_token",
			"token_expiry_date", "token_scope", "updated_at",
		})
		if err != nil {
			c.Error(e.Auth.WithDetail("Could not create/update user").CausedBy(err))
			c.Abort()
			return
		}

		c.Set("user", user)
		sesh.Set("USER", user.Username)

		// Successfully logged in
		c.Redirect(http.StatusSeeOther, "/")
	}
}

// Logout the current user, if logged in
func (cr *Auth) Logout() gin.HandlerFunc {
	return func(c *gin.Context) {
		if session.IsLoggedIn(c) {
			sesh := session.DefaultSession(c)
			sesh.Logout()
		}

		c.Redirect(http.StatusSeeOther, "/")
	}
}

// GuestPing replies to a guest's ping request with "pong"
func (cr *Auth) GuestPing() gin.HandlerFunc {
	return func(c *gin.Context) {
		// QUESTION: Should guest pinging return anything else?
		// NOTE: This tiny amount of work keeps the request blazing fast...

		c.JSON(http.StatusOK, models.Response{
			Data: "pong",
		})
	}
}
