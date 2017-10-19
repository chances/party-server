package controllers

import (
	"io"
  "fmt"

	e "github.com/chances/party-server/errors"
	"github.com/chances/party-server/events"
	"github.com/chances/party-server/session"
	"github.com/gin-gonic/gin"
)

// Events controller
type Events struct {
	Controller
}

// NewEvents creates a new Party controller
func NewEvents() Events {
	newEvents := Events{}
	newEvents.Setup()

	return newEvents
}

// Stream events to a client EventSource
func (cr *Events) Stream(ch string) gin.HandlerFunc {
	return func(c *gin.Context) {
		currentParty, err := session.CurrentParty(c)
		if err != nil {
			c.Error(e.BadRequest.WithDetail("User has not joined a party"))
			c.Abort()
		}

		channel := currentParty.RoomCode + ch

		listener := events.Listen(channel)
		defer events.StopListening(channel, listener)

		c.Stream(func(w io.Writer) bool {
		  message, _ := (<-listener).(string)
		  if message == "heartbeat" {
		    sseComment(c, message)
		    return true
      }

      // TODO: Provide some way to parameterize the event type? (name param, here)

			c.SSEvent(ch, message)
			return true
		})
	}
}

func sseComment(c *gin.Context, comment string) {
  c.Writer.WriteString(fmt.Sprintf(":%s\n", comment))
}
