package controllers

import (
	"database/sql"
	"net/http"

	e "github.com/chances/party-server/errors"
	"github.com/chances/party-server/models"
	"github.com/chances/party-server/session"
	"github.com/gin-gonic/gin"
)

// Queue controller
type Queue struct {
	Controller
}

// NewQueue creates a new Queue controller
func NewQueue() Queue {
	newQueue := Queue{}
	newQueue.Setup()

	return newQueue
}

// Get the current user's party's queue
func (cr *Queue) Get() gin.HandlerFunc {
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

		queueData, err := currentParty.QueueG().One()
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

		var queue []models.Track
		err = queueData.Data.Unmarshal(&queue)
		if err != nil {
			c.Error(e.Internal.CausedBy(err))
			c.Abort()
			return
		}

		c.JSON(http.StatusOK, models.NewResponse(
			currentParty.RoomCode+"#queue",
			"queue", cr.RequestURI(c),
			queue,
		))
	}
}
