package main

import (
	"log"
	"net/http"
	"time"

	"github.com/chances/chances-party/models"
	"github.com/garyburd/redigo/redis"
	raven "github.com/getsentry/raven-go"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
	"github.com/vattle/sqlboiler/queries/qm"
)

const (
	sessionDefaultKey = "github.com/chances/chances-party/session"
)

var (
	pool *redis.Pool
)

func newRedisPool() *redis.Pool {
	newPool := &redis.Pool{
		MaxActive:   10,
		MaxIdle:     2,
		IdleTimeout: 120 * time.Second,
		Dial: func() (redis.Conn, error) {
			return redis.DialURL(getenvOrFatal("REDIS_URL"))
		},
	}
	return newPool
}

func partySession() gin.HandlerFunc {
	return func(c *gin.Context) {
		s := loadSession(c)
		c.Set(sessionDefaultKey, s)

		// Try to get the session's user
		currentUsername, err := s.Get("USER")
		if err == nil {
			existingUser, err := models.UsersG(qm.Where("username = ?", currentUsername)).One()
			if err == nil {
				c.Set("user", existingUser)
			}
		}

		// If the session has a user, associate it with Sentry
		if IsLoggedIn(c) {
			user := CurrentUser(c)
			raven.SetUserContext(&raven.User{
				ID:       string(user.ID),
				Username: user.Username,
			})
		}

		maxAge := 60 * 60 * 12 // 12 hours
		if gin.IsDebugging() {
			c.SetCookie(s.name, s.ID, maxAge, "", "", false, true)
		} else {
			c.SetCookie(s.name, s.ID, maxAge, "", ".chancesnow.me", true, false)
		}

		c.Next()
	}
}

// TODO: Add session flash

// Session is a hella simple strng value session store for chances-party
type Session struct {
	name    string
	request *http.Request
	ID      string
	written bool
	writer  http.ResponseWriter
}

func loadSession(c *gin.Context) *Session {
	cookie, err := c.Cookie("cpSESSION")

	if err != nil {
		id := uuid.NewV4().String()
		return &Session{"cpSESSION", c.Request, id, false, c.Writer}
	}

	return &Session{"cpSESSION", c.Request, cookie, false, c.Writer}
}

// TODO: Serialize session into JSON

// Get returns the session value associated to the given key.
func (s *Session) Get(key string) (string, error) {
	c := pool.Get()
	defer c.Close()

	value, err := redis.String(c.Do("GET", s.ID+":"+key))

	if err != nil {
		println(err.Error())
		log.Printf("Could not GET %s\n", key)
		return "", err
	}

	return value, nil
}

// Set sets the session value associated to the given key.
func (s *Session) Set(key string, value string) error {
	c := pool.Get()
	defer c.Close()

	// status, err
	_, err := c.Do("SET", s.ID+":"+key, value)

	if err != nil {
		println(err.Error())
		log.Printf("Could not SET %s:%s\n", key, value)
		return err
	}

	return nil
}

// Delete removes the session value associated to the given key.
func (s *Session) Delete(key string) error {
	c := pool.Get()
	defer c.Close()

	_, err := c.Do("DEL", s.ID+":"+key)
	if err != nil {
		println(err.Error())
		log.Printf("Could not DEL %s\n", key)
		return err
	}

	return nil
}

// DefaultSession shortcut to get the session
func DefaultSession(c *gin.Context) *Session {
	return c.MustGet(sessionDefaultKey).(*Session)
}

// IsLoggedIn checks if a user is logged in to the session
func IsLoggedIn(c *gin.Context) bool {
	_, hasUser := c.Get("user")
	return hasUser
}

// CurrentUser shortcut to get the current session's user
func CurrentUser(c *gin.Context) *models.User {
	return c.MustGet("user").(*models.User)
}
