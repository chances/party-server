package main

import (
	"encoding/gob"
	"log"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/chances/chances-party/cache"
	"github.com/chances/chances-party/models"
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

	// === Data Stores ===
	// Postgres
	db = initDatabase()
	defer db.Close()
	// Redis
	pool = newRedisPool()
	defer pool.Close()
	// Cache
	partyCache = cache.NewStore(pool)

	gob.Register(models.CachedPlaylist{})
	gob.Register(models.Playlists{})
	gob.Register(models.Playlist{})
	gob.Register(models.Track{})
	gob.Register(models.TrackArtist{})

	setupAuth()

	// === Initialize Gin ===
	g := gin.New()

	// === Middleware ===
	g.Use(gin.Logger())
	// CORS
	corsOrigins :=
		strings.Split(getenv("CORS_ORIGINS", "https://chancesnow.me"), ",")
	g.Use(cors.New(cors.Config{
		AllowOrigins:     corsOrigins,
		AllowMethods:     []string{"GET", "PUT", "POST", "PATCH", "DELETE"},
		ExposeHeaders:    []string{"Content-Length", "Content-Type"},
		AllowCredentials: true,
		MaxAge:           12 * time.Hour,
	}))
	// Session
	g.Use(partySession(partyCache))

	g.Use(handleErrors())
	g.Use(gin.Recovery())

	// Static files
	g.Static("/css/", "./public")

	g.LoadHTMLGlob("views/*")

	// Application routes
	g.GET("/", func(c *gin.Context) {
		session := DefaultSession(c)
		flashedError, err := session.Error()
		if err != nil {
			c.Error(errInternal.CausedBy(err))
		}
		if IsLoggedIn(c) {
			currentUser := CurrentUser(c)
			var spotifyUser spotify.PrivateUser
			err := currentUser.SpotifyUser.Unmarshal(&spotifyUser)
			if err != nil {
				c.Error(errInternal.CausedBy(err))
				c.Abort()
				return
			}

			spotifyClient, err := ClientFromSession(c)
			if err != nil {
				c.Error(errInternal.CausedBy(err))
				c.Abort()
				return
			}

			var currentPlaylist *models.Playlist
			playlistsEntry, err := partyCache.GetOrDefer("playlists:"+currentUser.Username, func() cache.Entry {
				playlists := Playlists(*spotifyClient)
				return cache.Forever(playlists)
			})
			if err != nil {
				c.Error(errInternal.CausedBy(err))
				c.Abort()
				return
			}

			playlists := (*playlistsEntry.Value).(models.Playlists).Playlists
			for _, playlist := range playlists {
				if currentUser.SpotifyPlaylistID.String == playlist.ID {
					currentPlaylist = &playlist
					break
				}
			}
			c.HTML(http.StatusOK, "index.html", gin.H{
				"user":            spotifyUser,
				"currentPlaylist": currentPlaylist,
				"playlists":       playlists,
				"error":           flashedError,
			})
		} else {
			c.HTML(http.StatusOK, "index.html", gin.H{
				"error": flashedError,
			})
		}
	})

	playlist := g.Group("/playlist")
	playlist.Use(AuthRequired())
	{
		playlist.PATCH("", patchPlaylist)
	}

  g.Group("/search").Use(AuthRequired()).GET("", func(c *gin.Context) {
    query, exists := c.GetQuery("q")
    if !exists {
      query, exists = c.GetQuery("query")
    }
    if !exists {
      c.Error(errBadRequest.WithDetail("Malformed search, missing query"))
      c.Abort()
      return
    }

    spotifyClient, err := DefaultClient()
    if err != nil {
      c.Error(errInternal.CausedBy(err))
      c.Abort()
      return
    }
    result, err := spotifyClient.Search(query, spotify.SearchTypeTrack)
    if err != nil {
      c.Error(errInternal.CausedBy(err))
      c.Abort()
      return
    }

    c.JSON(http.StatusOK, models.Response{
      Data: gin.H{
        "tracks": result.Tracks.Tracks,
        "limit": result.Tracks.Limit,
        "offset": result.Tracks.Offset,
        "total": result.Tracks.Total,
      },
    })
  })

	g.GET("/auth/guest", func(c *gin.Context) {
		token, err := authGuest()
		if err != nil {
			c.Error(errUnauthorized.WithDetail("Could not create JWT").CausedBy(err))
			c.Abort()
			return
		}

		c.JSON(http.StatusOK, models.Response{
			Data: token,
		})
	})

	g.GET("/auth/login", login)
	g.GET("/auth/callback", spotifyCallback)
	g.GET("/auth/logout", func(c *gin.Context) {
		if IsLoggedIn(c) {
			session := DefaultSession(c)
			session.Logout()
		}

		c.Redirect(http.StatusSeeOther, "/")
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

func getenvOrFatal(key string) string {
	value := os.Getenv(key)
	if len(value) == 0 {
		log.Fatalf("error: Missing environment variable: %v\n", key)
	}
	return value
}
