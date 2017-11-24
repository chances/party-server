package middleware

import (
	"time"

	"github.com/gin-gonic/gin"
	cors "gopkg.in/gin-contrib/cors.v1"
)

// CORS handles cross origin resource security header stuffs
func CORS(allowedOrigins []string) gin.HandlerFunc {
	return cors.New(cors.Config{
		AllowOrigins:     allowedOrigins,
		AllowMethods:     []string{"GET", "PUT", "POST", "PATCH", "DELETE"},
		AllowHeaders:     []string{"Content-Length", "Content-Type", "Last-Event-ID"},
		ExposeHeaders:    []string{"Content-Length", "Content-Type", "Last-Event-ID"},
		AllowCredentials: true,
		MaxAge:           12 * time.Hour,
	})
}
