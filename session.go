package main

import (
	"errors"
	"log"
	"time"

	"github.com/chances/chances-party/cache"
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
	pool       *redis.Pool
	partyCache cache.Store
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

func partySession(store cache.Store) gin.HandlerFunc {
	return func(c *gin.Context) {
		s := loadSession(c, store)
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

// Session is a hella simple strng value session store for chances-party
type Session struct {
	name    string
	ID      string
	flashes map[string]string
	store   cache.Store
}

func loadSession(c *gin.Context, store cache.Store) *Session {
	cookie, err := c.Cookie("cpSESSION")

	if err != nil {
		id := uuid.NewV4().String()
		flashes := make(map[string]string)
		return &Session{"cpSESSION", id, flashes, store}
	}

	flashes := loadFlashes(cookie, store)
	return &Session{"cpSESSION", cookie, flashes, store}
}

func loadFlashes(id string, store cache.Store) map[string]string {
	flashesEntry, err := store.Get(id + ":flashes")
	if err != nil {
		return make(map[string]string)
	}

	flashes := *flashesEntry.Value
	return flashes.(map[string]string)
}

func (s *Session) saveFlashes() {
	if s.flashes == nil {
		return
	}

	err := s.store.Set(s.ID+":flashes", cache.Forever(s.flashes))
	if err != nil {
		log.Println(s.flashes)
		log.Fatal("cp flash:", err)
	}
}

// Get returns the session value associated to the given key.
func (s *Session) Get(key string) (string, error) {
	entry, err := s.store.Get(s.ID + ":" + key)
	if err != nil {
		return "", err
	}

	value := (*entry.Value).(string)
	return value, nil
}

// Set sets the session value associated to the given key.
func (s *Session) Set(key string, value string) error {
	return s.store.Set(s.ID+":"+key, cache.Forever(value))
}

// Flash a keyed value to session flash storage
func (s *Session) Flash(key string, value string) {
	s.flashes[key] = value
	s.saveFlashes()
}

// Flashes retrieves values from session flash storage
func (s *Session) Flashes(key ...string) ([]string, error) {
	numKeys := len(key)
	if numKeys == 0 {
		flashes := s.flashes
		val := make([]string, len(flashes))
		idx := 0
		for _, value := range flashes {
			val[idx] = value
			idx++
		}

		s.flashes = make(map[string]string)
		s.saveFlashes()

		return val, nil
	} else if numKeys == 1 {
		if val, ok := s.flashes[key[0]]; ok {
			valResult := make([]string, 1)
			valResult[0] = val

			delete(s.flashes, key[0])
			s.saveFlashes()

			return valResult, nil
		}

		return make([]string, 0), nil
	}

	return nil, errors.New("Flashes accepts only zero or one keys")
}

// Error retrieves an error from session flash storage
func (s *Session) Error() (string, error) {
	errors, err := s.Flashes("error")
	if err != nil {
		return "", err
	}
	if len(errors) > 0 {
		return errors[0], nil
	}

	return "", nil
}

// Delete removes the session value associated with the given key
func (s *Session) Delete(key string) error {
	return s.store.Delete(s.ID + ":" + key)
}

// Logout effectively ends a session by deleting user-land keys
func (s *Session) Logout() error {
	return s.Delete("USER")
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
