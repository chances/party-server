package middleware

import (
	"time"

	"github.com/gin-gonic/gin"
	cors "gopkg.in/gin-contrib/cors.v1"
)

// CORS handles cross origin resource security header stuffs
func CORS(allowedOrigins []string) gin.HandlerFunc {
	return func(c *gin.Context) {
		cors.New(cors.Config{
			AllowOrigins:     allowedOrigins,
			AllowMethods:     []string{"GET", "PUT", "POST", "PATCH", "DELETE"},
			ExposeHeaders:    []string{"Content-Length", "Content-Type"},
			AllowCredentials: true,
			MaxAge:           12 * time.Hour,
		})

		c.Next()
	}
}
