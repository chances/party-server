package controllers

import (
	"database/sql"
	"net/http"

	e "github.com/chances/party-server/errors"
	"github.com/chances/party-server/models"
	"github.com/chances/party-server/session"
	"github.com/gin-gonic/gin"
)

// History controller
type History struct {
	Controller
}

// NewHistory creates a new History controller
func NewHistory() History {
	newHistory := History{}
	newHistory.Setup()

	return newHistory
}

// Get the current user's party's History
func (cr *History) Get() gin.HandlerFunc {
	return func(c *gin.Context) {
		currentParty, err := session.CurrentParty(c)
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.JSON(http.StatusNotFound, models.EmptyRespose)
			return
		}

		historyData, err := currentParty.HistoryG().One()
		// TODO: Add pagination via JSON DB query
		if err != nil {
			if err != sql.ErrNoRows {
				c.Error(e.Internal.CausedBy(err))
				c.Abort()
				return
			}

			c.JSON(http.StatusNotFound, models.EmptyRespose)
			return
		}

		var history []models.Track
		err = historyData.Data.Unmarshal(&history)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		c.JSON(http.StatusOK, models.NewResponse(
			currentParty.RoomCode+"#history",
			"history", cr.RequestURI(c),
			history,
		))
	}
}
