package controllers

import (
	"net/http"

	"github.com/chances/chances-party/cache"
	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/models"
	s "github.com/chances/chances-party/spotify"
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
	"golang.org/x/oauth2/clientcredentials"
)

type Search struct {
	Controller
	spotifyAuth        spotify.Authenticator
	spotifyDefaultAuth clientcredentials.Config
}

func NewSearch(c cache.Store, auth spotify.Authenticator, defaultAuth clientcredentials.Config) Search {
	newSearch := Search{
		spotifyAuth:        auth,
		spotifyDefaultAuth: defaultAuth,
	}
	newSearch.Cache = c
	return newSearch
}

func (cr *Search) SearchTracks() gin.HandlerFunc {
	return func(c *gin.Context) {
		query, exists := c.GetQuery("q")
		if !exists {
			query, exists = c.GetQuery("query")
		}
		if !exists {
			c.Error(e.BadRequest.WithDetail("Malformed search, missing query"))
			c.Abort()
			return
		}

		spotifyClient, err := s.DefaultClient(cr.Cache, cr.spotifyDefaultAuth, cr.spotifyAuth)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}
		result, err := spotifyClient.Search(query, spotify.SearchTypeTrack)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		c.JSON(http.StatusOK, models.Response{
			Data: gin.H{
				"tracks": result.Tracks.Tracks,
				"limit":  result.Tracks.Limit,
				"offset": result.Tracks.Offset,
				"total":  result.Tracks.Total,
			},
		})
	}
}
