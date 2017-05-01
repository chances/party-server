package main

import (
	"log"
	"time"

	"github.com/chances/chances-party/models"
	"github.com/garyburd/redigo/redis"
	raven "github.com/getsentry/raven-go"
	"github.com/gin-contrib/sessions"
	"github.com/gin-gonic/gin"
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

func createSessionStore() sessions.Store {
	secret := []byte(getenvOrFatal("SESSION_SECRET"))
  if gin.IsDebugging() {
    return sessions.NewCookieStore(secret)
  }
	store, err := sessions.NewRedisStoreWithPool(pool, secret)
	if err != nil {
		log.Fatalln("Could not create Redis pool")
	}
	return store
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
