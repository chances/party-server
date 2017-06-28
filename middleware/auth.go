package middleware

import (
	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/session"
	"github.com/gin-gonic/gin"
)

// AuthRequired guards against unauthenticated sessions
func AuthRequired() gin.HandlerFunc {
	return func(c *gin.Context) {
		if !session.IsLoggedIn(c) {
			c.Error(e.Unauthorized)
			c.Abort()
			return
		}

		c.Next()
	}
}

// GuestsOnly guards against unauthorized guest sessions
func GuestsOnly() gin.HandlerFunc {
	return func(c *gin.Context) {
		if !session.IsGuest(c) {
			c.Error(e.Unauthorized)
			c.Abort()
			return
		}

		c.Next()
	}
}
