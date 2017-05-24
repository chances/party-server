package cache

import (
	"bytes"
	"encoding/gob"
	"time"

	"github.com/garyburd/redigo/redis"
)

// Store Party cache entries
type Store struct {
	pool *redis.Pool
}

// Entry stores a cache line given a value and expiry time
type Entry struct {
	Value  *interface{}
	Expiry time.Time
}

// IsExpired returns true if entry is expired
func (e *Entry) IsExpired() bool {
	return !e.Expiry.IsZero() && e.Expiry.Before(time.Now())
}

// Forever instructs cache to never evict an entry (Equal to Unix epoch)
var Forever time.Time = time.Unix(0, 0)

// NewStore instantiates a cache store given a Redis pool
func NewStore(p *redis.Pool) Store {
	gob.Register(Entry{})
	return Store{
		pool: p,
	}
}

// Exists checks for existence of a cache entry key
func (s *Store) Exists(key string) (bool, error) {
	return s.redisExists(key)
}

// Get a cache entry
func (s *Store) Get(key string) (*Entry, error) {
	valueBytes, err := s.redisGetBytes(key)
	if err != nil {
		return nil, err
	}

	var value *Entry
	valueBuffer := bytes.NewBuffer(valueBytes)
	dec := gob.NewDecoder(valueBuffer)
	err = dec.Decode(value)
	if err != nil {
		return nil, err
	}

	return value, nil
}

// GetOrDefer a cache entry, deferring to supplier func if entry doesn't exist
// or is expired. If key exists and is expired, entry is deleted
func (s *Store) GetOrDefer(key string, deferFn func() *interface{}) (*Entry, error) {
	now := time.Now()
	exists, err := s.redisExists(key)
	if err != nil {
		return nil, err
	}
	if !exists {
		return &Entry{
			Value: deferFn(),
		}, nil
	}

	entry, err := s.Get(key)
	if err != nil {
		return nil, err
	}

	if !entry.Expiry.IsZero() && entry.Expiry.Before(now) {
		go s.redisDelete(key)
		return &Entry{
			Value: deferFn(),
		}, nil
	}
	return entry, nil
}

// Set a cache entry
func (s *Store) Set(key string, e Entry) error {
	valueBuffer := new(bytes.Buffer)
	enc := gob.NewEncoder(valueBuffer)
	err := enc.Encode(e)
	if err != nil {
		return err
	}

	return s.redisSet(key, valueBuffer)
}

// Delete a cache entry
func (s *Store) Delete(key string) error {
	return s.redisDelete(key)
}
