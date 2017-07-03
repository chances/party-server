package controllers

import (
	"fmt"

	"github.com/chances/chances-party/cache"
	s "github.com/chances/chances-party/spotify"
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2/clientcredentials"
)

var (
	partyCache  cache.Store
	auth        spotify.Authenticator
	defaultAuth clientcredentials.Config
)

// SetCache sets the Party cache store used by controllers
func SetCache(c cache.Store) {
	partyCache = c
}

// SetSpotifyAuth sets the Spotify client factories used by controllers
func SetSpotifyAuth(spotifyAuth spotify.Authenticator, spotifyDefaultAuth clientcredentials.Config) {
	auth = spotifyAuth
	defaultAuth = spotifyDefaultAuth
}

// Controller template
type Controller struct {
	Cache              cache.Store
	SpotifyAuth        spotify.Authenticator
	SpotifyDefaultAuth clientcredentials.Config
}

// Setup a Controller with a Party cache store and Spotify client factories
func (cr *Controller) Setup() {
	cr.Cache = partyCache
	cr.SpotifyAuth = auth
	cr.SpotifyDefaultAuth = defaultAuth
}

// RequestURI gets the full request URI given a Gin request context
func (cr *Controller) RequestURI(c *gin.Context) string {
	if c.Request.URL.IsAbs() {
		return c.Request.URL.RequestURI()
	}

	return fmt.Sprintf("http://%s%s", c.Request.Host, c.Request.RequestURI)
}

// ClientFromSession gets a Spotify client from the session's user
func (cr *Controller) ClientFromSession(c *gin.Context) (*spotify.Client, error) {
	spotifyClient, err := s.ClientFromSession(c, cr.SpotifyAuth)
	if err != nil {
		return nil, err
	}
	return spotifyClient, nil
}
