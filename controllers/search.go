package controllers

import (
	"net/http"

	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/models"
	s "github.com/chances/chances-party/spotify"
	"github.com/gin-gonic/gin"
	"github.com/zmb3/spotify"
)

// Search controller
type Search struct {
	Controller
}

// NewSearch creates a new Search controller
func NewSearch() Search {
	newSearch := Search{}
	newSearch.Setup()
	return newSearch
}

// SearchTracks searches for Spotify tracks given a query string
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

		spotifyClient, err := s.DefaultClient(cr.SpotifyDefaultAuth, cr.SpotifyAuth)
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
