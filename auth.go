package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"

	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
	"golang.org/x/oauth2"
)

var (
	oauthConf *oauth2.Config
)

func setupOauth() {
	oauthConf = &oauth2.Config{
		ClientID:     getenvOrFatal("SPOTIFY_APP_KEY"),
		ClientSecret: getenvOrFatal("SPOTIFY_APP_SECRET"),
		RedirectURL:  getenvOrFatal("SPOTIFY_CALLBACK"),
		Scopes: []string{
			"user-read-email",
			"user-read-private",
			"playlist-read-private",
			"playlist-read-collaborative",
		},
		Endpoint: oauth2.Endpoint{
			AuthURL:  "https://accounts.spotify.com/authorize",
			TokenURL: "https://accounts.spotify.com/api/token",
		},
	}
}

func login(c *gin.Context) {
	RedirectURL, err := url.Parse(oauthConf.Endpoint.AuthURL)
	if err != nil {
		log.Fatal("Parse: ", err)
	}
	var params = url.Values{}
	params.Add("client_id", oauthConf.ClientID)
	params.Add("scope", strings.Join(oauthConf.Scopes, " "))
	params.Add("redirect_uri", oauthConf.RedirectURL)
	params.Add("response_type", "code")

	state := uuid.NewV4().String()
	params.Add("state", state)

	session := sessions.Default(c)
	session.Set("AUTH_STATE", state)
	session.Save()

	RedirectURL.RawQuery = params.Encode()
	c.Redirect(http.StatusSeeOther, RedirectURL.String())
}

type spotifyUser struct {
	ID          string
	Email       string
	DisplayName string `json:"display_name"`
	ProfileURL  string
	Href        string
	URI         string
	Product     string
	Country     string
	Followers   struct {
		Total int
	}
	ExternalURLs struct {
		Spotify string
	} `json:"external_urls"`
	Images []struct {
		URL    string
		Width  int
		Height int
	}
}

func spotifyCallback(c *gin.Context) {
	session := sessions.Default(c)
	callbackState := session.Get("AUTH_STATE")
	session.Delete("AUTH_STATE")
	session.Save()

	state := c.Query("state")
	if state != callbackState {
		redirectWithError(c, session, "Invalid oauth state", nil)
		return
	}

	code := c.Query("code")
	token, err := oauthConf.Exchange(context.Background(), code)
	if err != nil {
		redirectWithError(c, session, "Token request failed", err)
		return
	}

	tokenRequest := spotifyGet("me", token)
	resp, err := http.DefaultClient.Do(tokenRequest)
	if err != nil {
		redirectWithError(c, session, "Could not get user", err)
		return
	}

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatalln(err)
	}

	var user spotifyUser
	if json.Unmarshal(body, &user) != nil {
		redirectWithError(c, session, "Could not parse user", err)
		return
	}

	session.AddFlash(user, "user")
	session.Save()
	c.Redirect(http.StatusSeeOther, "/")
}

func redirectWithError(c *gin.Context, session sessions.Session, message string, err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s: %s\n", message, err)
	} else {
		fmt.Fprintf(os.Stderr, "%s\n", message)
	}
	session.AddFlash("Login was unsuccessful: "+message, "error")
	session.Save()
	c.Redirect(http.StatusSeeOther, "/")
}

func spotifyGet(resource string, token *oauth2.Token) *http.Request {
	req, err := http.NewRequest("GET", "https://api.spotify.com/v1/"+resource, nil)
	if err != nil {
		log.Fatalln(err)
	}

	token.SetAuthHeader(req)
	return req
}
