package main

import (
	"context"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strings"
	"time"

	"github.com/chances/chances-party/cache"
	"github.com/chances/chances-party/models"
	"github.com/dgrijalva/jwt-go"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/clientcredentials"
)

var (
	auth          spotify.Authenticator
	scopes        string
	defaultAuth   clientcredentials.Config
	jwtSigningKey []byte
)

func setupAuth() {
	gob.Register(oauth2.Token{})

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

	// Spotify Client Credentials auth flow
	defaultAuth = clientcredentials.Config{
		ClientID:     getenvOrFatal("SPOTIFY_APP_KEY"),
		ClientSecret: getenvOrFatal("SPOTIFY_APP_SECRET"),
		TokenURL:     "https://accounts.spotify.com/api/token",
	}

	jwtSigningKey = []byte(getenvOrFatal("JWT_SECRET"))
}

// AuthRequired guards against unauthenticated sessions
func AuthRequired() gin.HandlerFunc {
	return func(c *gin.Context) {
		// TODO: Check for JWT, if available, otherwise...

		if !IsLoggedIn(c) {
			c.Error(errUnauthorized)
			c.Abort()
			return
		}

		c.Next()
	}
}

func authGuest() (string, error) {
	// TODO: Auth a guest session given a party "room" or after OAuth

	jwtSessionID := uuid.NewV4().String()
	// TODO: Store token ID is Redis with expiration
	// TODO: Goroutine to clean expired JWTs
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"id": jwtSessionID,
	})

	tokenString, err := token.SignedString(jwtSigningKey)
	if err != nil {
		return "", err
	}
	return tokenString, nil
}

func validateJwt(tokenString string) (string, bool, error) {
	token, err := jwt.Parse(tokenString, func(token *jwt.Token) (interface{}, error) {
		if token.Header["alg"] != jwt.SigningMethodHS256.Alg() {
			return nil, fmt.Errorf("Unexpected signing method: %v", token.Header["alg"])
		}

		return jwtSigningKey, nil
	})
	if err != nil {
		return "", false, err
	}

	if claims, ok := token.Claims.(jwt.MapClaims); ok && token.Valid {
		return claims["id"].(string), token.Valid, nil
	}

	return "", token.Valid, nil
}

func defaultToken() (*oauth2.Token, error) {
	tokenEntry, err := partyCache.GetOrDefer("SPOTIFY_TOKEN", func() cache.Entry {
		token, err := defaultAuth.Token(context.Background())
		if err != nil {
			log.Fatalf("spotify client credentials: %v\n", err)
		}
		log.Println(token)
		return cache.Expires(token.Expiry, *token)
	})
	if err != nil {
		return nil, err
	}

  token := (*tokenEntry.Value).(oauth2.Token)
	return &token, nil
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
