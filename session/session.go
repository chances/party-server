package session

import (
	"errors"
	"log"

	"github.com/chances/party-server/cache"
)

const (
	sessionDefaultKey = "github.com/chances/party-server/session"
)

// Session is a hella simple string value session store for party-server
type Session struct {
	name    string
	ID      string
	flashes map[string]string
	store   cache.Store
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

	value := entry.Value.(string)
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
	flashErrors, err := s.Flashes("error")
	if err != nil {
		return "", err
	}
	if len(flashErrors) > 0 {
		return flashErrors[0], nil
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
