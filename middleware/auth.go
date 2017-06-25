package middleware

import (
	e "github.com/chances/chances-party/errors"
	"github.com/chances/chances-party/session"
	"github.com/gin-gonic/gin"
)

var authDisabled = false

// AuthRequired guards against unauthenticated sessions
func AuthRequired() gin.HandlerFunc {
	return func(c *gin.Context) {
		// TODO: Check for JWT, if available, otherwise...

		if !authDisabled && !gin.IsDebugging() && !session.IsLoggedIn(c) {
			c.Error(e.Unauthorized)
			c.Abort()
			return
		}

		c.Next()
	}
}

// SetAuthDisabled sets whether or not authentication is checked
func SetAuthDisabled(disabled bool) {
	authDisabled = disabled
}
