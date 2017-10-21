package spotify

import (
	"context"
	"log"

	"github.com/chances/party-server/cache"
	"github.com/chances/party-server/session"
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2"
	"golang.org/x/oauth2/clientcredentials"
)

var partyCache cache.Store

// SetCache sets the Party cache store used by this Spotify client
func SetCache(c cache.Store) {
	partyCache = c
}

// DefaultToken gets a Spotify access token via Spotify's client credentials OAuth flow
func DefaultToken(auth clientcredentials.Config) (*oauth2.Token, error) {
	tokenEntry, err := partyCache.GetOrDefer("SPOTIFY_TOKEN", func() (*cache.Entry, error) {
		token, err := auth.Token(context.Background())
		if err != nil {
			log.Fatalf("spotify client credentials: %v\n", err)
		}
		log.Println(token)
		tokenEntry := cache.Expires(token.Expiry, *token)
		return &tokenEntry, nil
	})
	if err != nil {
		return nil, err
	}

	token := (*tokenEntry).Value.(oauth2.Token)
	return &token, nil
}

// DefaultClient gets a Spotify client from the default token
// The default token is provided via Spotify's Client Credentials auth flow
func DefaultClient(def clientcredentials.Config, auth spotify.Authenticator) (*spotify.Client, error) {
	token, err := DefaultToken(def)
	if err != nil {
		return nil, err
	}

	newClient := auth.NewClient(token)
	return &newClient, nil
}

// ClientFromSession gets a Spotify client from the session's user
func ClientFromSession(c *gin.Context, auth spotify.Authenticator) (*spotify.Client, error) {
	user := session.CurrentUser(c)
	newClient := auth.NewClient(&oauth2.Token{
		AccessToken:  user.AccessToken,
		RefreshToken: user.RefreshToken,
		Expiry:       user.TokenExpiryDate,
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
