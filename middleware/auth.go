package middleware

import (
	e "github.com/chances/party-server/errors"
	"github.com/chances/party-server/session"
	"github.com/gin-gonic/gin"
)

// AuthenticationRequired guards against unauthenticated sessions
func AuthenticationRequired() gin.HandlerFunc {
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

// AuthorizationRequired guards against any unauthorized session
//
// A user must either be a guest or authenticated via OAuth
func AuthorizationRequired() gin.HandlerFunc {
	return func(c *gin.Context) {
		if (!session.IsLoggedIn(c)) && (!session.IsGuest(c)) {
			c.Error(e.Unauthorized)
			c.Abort()
			return
		}

		c.Next()
	}
}
