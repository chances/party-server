package main

import (
	"log"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/joho/godotenv"
	"github.com/zmb3/spotify"
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
	defer pool.Close()

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
	g.Use(partySession())

	g.Use(handleErrors())
	g.Use(gin.Recovery())

	// Static files
	g.Static("/css/", "./public")

	g.LoadHTMLGlob("views/*")

	// Application routes
	g.GET("/", func(c *gin.Context) {
		// session := DefaultSession(c)
		// TODO: Read errors from session flash
		// data := map[string]interface{}{}
		// if flashes := session.Flashes("error"); len(flashes) > 0 {
		// 	data["error"] = flashes[0]
		// }
		if IsLoggedIn(c) {
			currentUser := CurrentUser(c)
			var spotifyUser spotify.PrivateUser
			err := currentUser.SpotifyUser.Unmarshal(&spotifyUser)
			if err != nil {
				c.Error(err)
				c.Abort()
				return
			}

			c.HTML(http.StatusOK, "index.html", gin.H{
				"user":  spotifyUser,
				"error": "",
			})
		} else {
			c.HTML(http.StatusOK, "index.html", gin.H{})
		}
	})
	g.GET("/auth/login", login)
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
