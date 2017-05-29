package session

import (
	"github.com/chances/chances-party/cache"
	"github.com/chances/chances-party/models"
	"github.com/getsentry/raven-go"
	"github.com/gin-gonic/gin"
	"github.com/twinj/uuid"
	"github.com/vattle/sqlboiler/queries/qm"
)

// Middleware creates the Party session middleware
func Middleware(store cache.Store) gin.HandlerFunc {
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
			c.SetCookie(s.name, s.ID, maxAge, "", ".chancesnow.me", true, true)
		}

		c.Next()
	}
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

	flashes := flashesEntry.Value
	return flashes.(map[string]string)
}
