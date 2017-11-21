package main

import (
	"encoding/gob"
	"log"
	"os"
	"strings"

	"github.com/chances/party-server/cache"
	"github.com/chances/party-server/controllers"
	e "github.com/chances/party-server/errors"
	m "github.com/chances/party-server/middleware"
	"github.com/chances/party-server/models"
	"github.com/chances/party-server/session"
	s "github.com/chances/party-server/spotify"
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
		getenvOrFatal("GUEST_SECRET"),
	)
	index := controllers.NewIndex()
	party := controllers.NewParty()
	playlists := controllers.NewPlaylists()
	search := controllers.NewSearch()
	events := controllers.NewEvents()

	go party.PruneExpiredGuests()

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
	parties.Use(m.AuthenticationRequired())
	{
		parties.POST("/start", party.Start())
		// parties.POST("/end", party.End())
	}
	g.Group("/party").
		Use(m.AuthorizationRequired()).
		GET("", party.Get())
	g.POST("/party/join", party.Join())

	// Playlist routes
	playlist := g.Group("/playlist")
	playlist.Use(m.AuthenticationRequired())
	{
		playlist.GET("", playlists.Get())
		playlist.PATCH("", playlists.Patch())
	}

	// Search routes
	g.Group("/search").
		Use(m.AuthorizationRequired()).
		GET("", search.SearchTracks())

	// Events routes
	event := g.Group("/events")
	event.Use(m.AuthorizationRequired())
	{
		event.GET("/party", events.Stream("party"))
	}

	// Authentication routes
	g.Group("/auth/ping").
		Use(m.GuestsOnly()).
		GET("", auth.GuestPing())
	g.Group("/auth/token").
		Use(m.AuthenticationRequired()).
		GET("", auth.GetToken())

	g.GET("/auth/login", auth.Login())
	g.GET("/auth/callback", auth.SpotifyCallback())
	g.GET("/auth/finished", auth.Finished())
	g.Group("/auth/logout").
		Use(m.AuthenticationRequired()).
		GET("", auth.Logout())

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
