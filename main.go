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
	"github.com/getsentry/raven-go"
	"github.com/gin-contrib/gzip"
	"github.com/gin-contrib/sentry"
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
	)
	index := controllers.NewIndex()
	party := controllers.NewParty()
	playlists := controllers.NewPlaylists()
	queue := controllers.NewQueue()
	history := controllers.NewHistory()
	music := controllers.NewPlayback()
	search := controllers.NewSearch()
	events := controllers.NewEvents()

	go party.PruneExpiredGuests()

	// === Initialize Gin ===
	g := gin.New()

	// === Middleware ===
	g.Use(gin.Logger())
	g.Use(gzip.Gzip(gzip.DefaultCompression))
	// CORS
	corsOrigins :=
		strings.Split(getenv("CORS_ORIGINS", "https://chancesnow.me"), ",")
	g.Use(m.CORS(corsOrigins))
	// Session
	g.Use(session.Middleware(partyCache))

	g.Use(e.HandleErrors())
	g.Use(sentry.Recovery(raven.DefaultClient, false))

	// Static files
	g.Static("/css/", "./public")

	// Templates
	g.LoadHTMLGlob("views/*")

	// === Application routes ===
	g.GET("/", index.Get())

	// Party routes
	g.POST("/party/join", party.Join())
	partyAdmin := g.Group("/party").Use(m.AuthenticationRequired())
	{
		partyAdmin.POST("/start", party.Start())
		partyAdmin.POST("/end", party.End())
	}
	partyUser := g.Group("/party").Use(m.AuthorizationRequired())
	{
		partyUser.GET("", party.Get())
		partyUser.GET("/queue", queue.Get())
		partyUser.GET("/history", history.Get())
	}

	// Playlist routes
	playlist := g.Group("/playlist")
	playlist.Use(m.AuthenticationRequired())
	{
		playlist.GET("", playlists.Get())
		playlist.PATCH("", playlists.Patch())
	}

	// Playback routes
	playback := g.Group("/playback").Use(m.AuthenticationRequired())
	{
		playback.POST("/play", music.Play())
		playback.POST("/pause", music.Pause())
		playback.POST("/skip", music.Skip())
	}

	// Search routes
	g.Group("/search").Use(m.AuthorizationRequired()).
		GET("", search.SearchTracks())

	// Events routes
	event := g.Group("/events").Use(m.AuthorizationRequired())
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
	g.GET("/auth/mobile", auth.Mobile())
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
