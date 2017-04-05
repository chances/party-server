package main

import (
	"log"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
	"github.com/joho/godotenv"
	"gopkg.in/gin-contrib/cors.v1"
)

func main() {
	dotenvErr := godotenv.Load()
	if dotenvErr != nil {
		log.Println("Warning: .env file is not present. Using system provided environment variables")
	}

	setupAuth()

	// === Data Stores ===
	// Postgres
	db = initDatabase()
	defer db.Close()
	// Redis
	pool = newRedisPool()

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
	// Session
	g.Use(sessions.Sessions("cpSESSION", createSessionStore()))
	g.Use(configureSession())

	g.Use(gin.Recovery())

	// Static files
	g.Static("/css/", "./public")

	// Application routes
	g.GET("/", func(c *gin.Context) {
		session := sessions.Default(c)
		data := map[string]interface{}{}
		if flashes := session.Flashes("error"); len(flashes) > 0 {
			data["error"] = flashes[0]
		}
		if flashes := session.Flashes("user"); len(flashes) > 0 {
			data["user"] = flashes[0]
		}
		log.Printf("User: %s\n", data)
		c.Header("Content-Type", "text/html; charset=utf-8")
		c.String(http.StatusOK, "<p><a href=\"/login\">Login with Spotify</a></p><p>Error: None?</p>")
	})
	g.GET("/login", login)
	g.GET("/auth/callback", spotifyCallback)

	g.Run()
}

func getenv(key, fallback string) string {
	value := os.Getenv(key)
	if len(value) == 0 {
		return fallback
	}
	return value
}

func getenvOrFatal(key string) string {
	value := os.Getenv(key)
	if len(value) == 0 {
		log.Fatalf("error: Missing environment variable: %v\n", key)
	}
	return value
}
