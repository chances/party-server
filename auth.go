package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"net/http"
	"os"
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
	session.Save()

	c.Redirect(http.StatusSeeOther, auth.AuthURL(state))
}

// IDEA: Convert error handling shenanigans to Observable chain
func spotifyCallback(c *gin.Context) {
	session := sessions.Default(c)
	sessionState, ok := session.Get("AUTH_STATE").(string)
	log.Println(sessionState)
	if !ok {
		c.Error(errors.New("Token request failed, invalid state"))
		// TODO: In error handling middleware, add Authorization header for Unauthorized responses
		c.Status(http.StatusUnauthorized)
		return
	}
	session.Delete("AUTH_STATE")
	session.Save()

	// Validate OAuth state
	oauthState := c.Request.FormValue("state")
	if oauthState != sessionState {
		c.Error(errors.New("Auth failed, mismatched state"))
		// TODO: In error handling middleware, add Authorization header for Unauthorized responses
		c.Status(http.StatusUnauthorized)
		return
	}

	// Retrieve token
	token, err := auth.Token(sessionState, c.Request)
	if err != nil {
		c.Error(errors.New("Token request failed"))
		// TODO: In error handling middleware, flash error to session if redirecting
		c.Redirect(http.StatusSeeOther, "/")
		return
	}

	// Get logged in user
	client := auth.NewClient(token)
	spotifyUser, err := client.CurrentUser()
	if err != nil {
		c.Error(errors.New("Could not get user"))
		// TODO: In error handling middleware, flash error to session if redirecting
		c.Redirect(http.StatusSeeOther, "/")
		return
	}

	spotifyUserJSON, err := json.Marshal(spotifyUser)
	if err != nil {
		c.Error(errors.New("Could not serialize user"))
		// TODO: In error handling middleware, flash error to session if redirecting
		c.Redirect(http.StatusSeeOther, "/")
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
			c.Error(errors.New("Could not update user"))
			// TODO: In error handling middleware, flash error to session if redirecting
			c.Redirect(http.StatusSeeOther, "/")
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
			c.Error(errors.New("Could not create user"))
			// TODO: In error handling middleware, flash error to session if redirecting
			c.Redirect(http.StatusSeeOther, "/")
			return
		}
	}

	// Successfully logged in
	c.Redirect(http.StatusSeeOther, "/")
}

func redirectWithError(c *gin.Context, session sessions.Session, message string, err error) {
	// TOOD: Move this to general error handler middleware
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s\n", message, err)
	} else {
		fmt.Fprintf(os.Stderr, "%s\n", message)
	}
	session.AddFlash("Could not login to Spotify: "+message, "error")
	session.Save()
	c.Redirect(http.StatusSeeOther, "/")
}
