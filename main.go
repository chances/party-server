package main

import (
	"encoding/gob"
	"log"
	"os"
	"strings"

	"github.com/chances/chances-party/cache"
	"github.com/chances/chances-party/controllers"
	e "github.com/chances/chances-party/errors"
	m "github.com/chances/chances-party/middleware"
	"github.com/chances/chances-party/models"
	"github.com/chances/chances-party/session"
	s "github.com/chances/chances-party/spotify"
	"github.com/gin-gonic/gin"
	"github.com/joho/godotenv"
)

func main() {
	dotenvErr := godotenv.Load()
	if dotenvErr != nil {
		log.Println("Warning: .env file is not present. Using system provided environment variables")
	}

	// === Data Stores ===
	// Postgres
	db := initDatabase()
	defer db.Close()
	// Redis
	pool = newRedisPool()
	defer pool.Close()
	// Cache
	partyCache = cache.NewStore(pool)

	gob.Register(models.Track{})
	gob.Register(models.TrackArtist{})

	s.SetCache(partyCache)
	controllers.SetCache(partyCache)

	// App controllers
	auth := controllers.NewAuth(
		getenvOrFatal("SPOTIFY_APP_KEY"),
		getenvOrFatal("SPOTIFY_APP_SECRET"),
		getenvOrFatal("SPOTIFY_CALLBACK"),
		getenvOrFatal("JWT_SECRET"),
	)
	index := controllers.NewIndex()
	party := controllers.NewParty()
	playlists := controllers.NewPlaylists()
	search := controllers.NewSearch()

	// === Initialize Gin ===
	g := gin.New()

	// === Middleware ===
	g.Use(gin.Logger())
	// CORS
	corsOrigins :=
		strings.Split(getenv("CORS_ORIGINS", "https://chancesnow.me"), ",")
	g.Use(m.CORS(corsOrigins))
	// Session
	g.Use(session.Middleware(partyCache))

	g.Use(e.HandleErrors())
	g.Use(gin.Recovery())

	// Static files
	g.Static("/css/", "./public")

	// Templates
	g.LoadHTMLGlob("views/*")

	// === Application routes ===
	g.GET("/", index.Get())

	// Party routes
	parties := g.Group("/party")
	parties.Use(m.AuthRequired())
	{
		parties.GET("", party.Get())
		parties.POST("/start", party.Start())
	}
	g.POST("/party/join", party.Join())

	// Playlist routes
	playlist := g.Group("/playlist")
	playlist.Use(m.AuthRequired())
	{
		playlist.GET("", playlists.Get())
		playlist.PATCH("", playlists.Patch())
	}

	// Search routes
	g.Group("/search").
		Use(m.AuthRequired()).
		GET("", search.SearchTracks())

	g.GET("/auth/guest", auth.Guest())

	g.GET("/auth/login", auth.Login())
	g.GET("/auth/callback", auth.SpotifyCallback())
	g.GET("/auth/logout", auth.Logout())

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
