package main

import (
	"bytes"
	"encoding/gob"
	"errors"
	"fmt"
	"log"
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

func redisGet(key string) (string, error) {
	c := pool.Get()
	defer c.Close()

	value, err := redis.String(c.Do("GET", key))

	if err != nil {
		println(err.Error())
		log.Printf("Could not GET %s\n", key)
		return "", err
	}

	return value, nil
}

func redisSet(key string, value string) error {
	c := pool.Get()
	defer c.Close()

	// status, err
	_, err := c.Do("SET", key, value)

	if err != nil {
		println(err.Error())
		log.Printf("Could not SET %s:%s\n", key, value)
		return err
	}

	return nil
}

func redisExists(key string) (bool, error) {
	c := pool.Get()
	defer c.Close()

	ok, err := redis.Bool(c.Do("EXISTS", key))
	if err != nil {
		return ok, fmt.Errorf("Could not determine if key %s exists: %v", key, err)
	}
	return ok, err
}

// Session is a hella simple strng value session store for chances-party
type Session struct {
	name    string
	ID      string
	flashes map[string]interface{}
}

func loadSession(c *gin.Context) *Session {
	cookie, err := c.Cookie("cpSESSION")

	if err != nil {
		id := uuid.NewV4().String()
		flashes := make(map[string]interface{})
		return &Session{"cpSESSION", id, flashes}
	}

	return &Session{"cpSESSION", cookie, loadFlashes(cookie)}
}

// TODO: Serialize session into JSON
func loadFlashes(id string) map[string]interface{} {
	c := pool.Get()
	defer c.Close()

	value, err := redis.Bytes(c.Do("GET", id+":flashes"))
	if err != nil {
		return make(map[string]interface{})
	}

	var flashes map[string]interface{}
	flashesBuffer := bytes.NewBuffer(value)
	dec := gob.NewDecoder(flashesBuffer)
	err = dec.Decode(&flashes)
	if err != nil {
		return make(map[string]interface{})
	}
	return flashes
}

func (s *Session) saveFlashes() {
	if s.flashes == nil {
		return
	}

	flashesBuffer := new(bytes.Buffer)
	enc := gob.NewEncoder(flashesBuffer)
	err := enc.Encode(s.flashes)
	if err != nil {
		log.Fatal("cp flash:", err)
	}

	c := pool.Get()
	defer c.Close()

	// status, err
	_, err = c.Do("SET", s.ID+":flashes", flashesBuffer.Bytes())
	if err != nil {
		log.Fatal("cp flash:", err)
	}
}

// Get returns the session value associated to the given key.
func (s *Session) Get(key string) (string, error) {
	value, err := redisGet(s.ID + ":" + key)
	if err != nil {
		return "", err
	}

	return value, nil
}

// Set sets the session value associated to the given key.
func (s *Session) Set(key string, value string) error {
	err := redisSet(s.ID+":"+key, value)
	if err != nil {
		return err
	}

	return nil
}

// Flash a keyed value to session flash storage
func (s *Session) Flash(key string, value interface{}) {
	s.flashes[key] = value
	s.saveFlashes()
}

// Flashes retrieves values from session flash storage
func (s *Session) Flashes(key ...string) ([]interface{}, error) {
	numKeys := len(key)
	if numKeys == 0 {
		flashes := s.flashes
		val := make([]interface{}, len(flashes))
		idx := 0
		for _, value := range flashes {
			val[idx] = value
			idx++
		}

		s.flashes = make(map[string]interface{})
		s.saveFlashes()

		return val, nil
	} else if numKeys == 1 {
		if val, ok := s.flashes[key[0]]; ok {
			valResult := make([]interface{}, 1)
			valResult[0] = val

			delete(s.flashes, key[0])
			s.saveFlashes()

			return valResult, nil
		}

		return make([]interface{}, 0), nil
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
		return errors[0].(string), nil
	}

	return "", nil
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
