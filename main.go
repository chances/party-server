package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/joho/godotenv"
	"gopkg.in/gin-contrib/cors.v1"
)

func main() {
	dotenvErr := godotenv.Load()
	if dotenvErr != nil {
		log.Println("Warning: .env file is not present. Using system provided environment variables")
	}

	g := gin.New()
	g.Use(gin.Logger())

	// === Middleware ===
	// CORS
	corsOrigins :=
		strings.Split(getenv("CORS_ORIGINS", "https://chancesnow.me"), ",")
	g.Use(cors.New(cors.Config{
		AllowOrigins:     corsOrigins,
		AllowMethods:     []string{"GET", "PUT", "POST", "DELETE"},
		ExposeHeaders:    []string{"Content-Length"},
		AllowCredentials: true,
		MaxAge:           12 * time.Hour,
	}))
	// Static files
	g.Static("/css/", "./public")

	g.Use(gin.Recovery())

	// Application routes
	g.GET("/", func(c *gin.Context) {
		c.String(http.StatusOK, "Hello, world!")
	})

	g.Run()
}

func getenv(key, fallback string) string {
	value := os.Getenv(key)
	if len(value) == 0 {
		return fallback
	}
	return value
}

func getenvOrFail(key string) string {
	value := os.Getenv(key)
	if len(value) == 0 {
		fmt.Fprintf(os.Stderr, "error: Missing environment variable: %v\n", key)
		os.Exit(1)
	}
	return value
}
