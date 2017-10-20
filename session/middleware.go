package session

import (
	"errors"
	"time"

	"github.com/chances/party-server/cache"
	"github.com/chances/party-server/models"
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

		// Try to get the session's guest Party Access Token
		currentGuestToken, err := s.Get("GUEST")
		if err == nil {
			guestMetadata, err := validateGuestSession(currentGuestToken, c, store, s)
			if err == nil {
				c.Set("guest", guestMetadata)
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

// IsGuest checks if a user is authenticated as a guest to the session
func IsGuest(c *gin.Context) bool {
	_, hasGuest := c.Get("guest")
	return hasGuest
}

// CurrentUser shortcut to get the current session's user
func CurrentUser(c *gin.Context) *models.User {
	return c.MustGet("user").(*models.User)
}

// CurrentGuest shortcut to get the current session's guest
func CurrentGuest(c *gin.Context) *gin.H {
	return c.MustGet("guest").(*gin.H)
}

// CurrentParty shortcut to get the current session's party
func CurrentParty(c *gin.Context) (*models.Party, error) {
	var party *models.Party
	var err error

	if IsGuest(c) {
		currentGuest := *CurrentGuest(c)
		party, err = models.FindPartyG(currentGuest["Party"].(int))
	} else if IsLoggedIn(c) {
		currentUser := CurrentUser(c)
		party, err = currentUser.PartyG().One()
	}
	if err != nil {
		return nil, err
	}

	return party, nil
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

// Validate the guest's session hasn't expired and this request's
//  Origin matches the guest's originating origin
func validateGuestSession(token string, c *gin.Context, store cache.Store, s *Session) (*gin.H, error) {
	// A guest's session ends when one of the following conditions are met:
	//  - the guest's session is expired
	//  - the guest's origin entry is nil, or
	//  - this request is invalid
	origin := c.Request.Header.Get("Origin")
	guestEntry, err := store.Get(token)
	if origin == "" || err != nil {
		s.Delete("GUEST")
		if err == nil {
			store.Delete(token)
		}

		return nil, errors.New("Guest session is invalid")
	}

	var guestMetadata gin.H
	guestMetadata = guestEntry.Value.(gin.H)
	guestOrigin, originKeyExists := guestMetadata["Origin"]

	if guestEntry.IsExpired() || !originKeyExists ||
		origin != guestOrigin.(string) {
    s.Delete("GUEST")
		store.Delete(token)

		return nil, errors.New("Guest session is invalid")
	}

	// The guest session is valid, refresh the guest's cache entry
	guestMetadata["Origin"] = origin
	store.Set(token, cache.Expires(
		time.Now().Add(time.Minute*time.Duration(30)),
		guestMetadata,
	))

	return &guestMetadata, nil
}

func loadFlashes(id string, store cache.Store) map[string]string {
	flashesEntry, err := store.Get(id + ":flashes")
	if err != nil {
		return make(map[string]string)
	}

	flashes := flashesEntry.Value
	return flashes.(map[string]string)
}
