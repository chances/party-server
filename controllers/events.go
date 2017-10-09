package controllers

import (
  "io"

  "github.com/chances/party-server/events"
  "github.com/chances/party-server/session"
  e "github.com/chances/party-server/errors"
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
      c.SSEvent("message", <-listener)
      return true
    })
	}
}
