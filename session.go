package main

import (
	"log"
	"time"

	"github.com/antonlindstrom/pgstore"
	"github.com/chances/chances-party/models"
	"github.com/garyburd/redigo/redis"
	raven "github.com/getsentry/raven-go"
	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
	gsessions "github.com/gorilla/sessions"
)

var (
	pool *redis.Pool
)

func newRedisPool() *redis.Pool {
	return &redis.Pool{
		MaxActive:   10,
		MaxIdle:     2,
		IdleTimeout: 120 * time.Second,
		Dial: func() (redis.Conn, error) {
			return redis.DialURL(getenvOrFatal("REDIS_URL"))
		},
	}
}

func createSessionStore() postgresStore {
	secret := []byte(getenvOrFatal("SESSION_SECRET"))
	store, err := pgstore.NewPGStoreFromPool(db.DB, secret)
	if err != nil {
		log.Fatalln("Could not create Redis pool")
	}

	defer store.StopCleanup(store.Cleanup(time.Minute * 5))

	return &pgStore{store}
}

type postgresStore interface {
	sessions.Store
}

type pgStore struct {
	*pgstore.PGStore
}

func (c *pgStore) Options(options sessions.Options) {
	c.PGStore.Options = &gsessions.Options{
		Path:     options.Path,
		Domain:   options.Domain,
		MaxAge:   options.MaxAge,
		Secure:   options.Secure,
		HttpOnly: options.HttpOnly,
	}
}

func configureSession() gin.HandlerFunc {
	return func(c *gin.Context) {
		session := sessions.Default(c)
		if gin.IsDebugging() {
			session.Options(sessions.Options{
				HttpOnly: true,
			})
		} else {
			session.Options(sessions.Options{
				Domain:   ".chancesnow.me",
				Secure:   true,
				HttpOnly: false,
			})
		}

		// If the session has a user, associate it with Sentry
		u, hasUser := c.Get("user")
		if hasUser {
			user, ok := u.(models.User)
			if ok {
				raven.SetUserContext(&raven.User{
					ID:       string(user.ID),
					Username: user.Username,
				})
			}
		}

		c.Next()
	}
}
