package cache

import (
	"bytes"
	"encoding/gob"
	"log"
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
	isForever := e.Expiry.Equal(time.Unix(0, 0))
	return !isForever && e.Expiry.Before(time.Now())
}

// Value creates a plain cache entry given a value
func Value(value interface{}) Entry {
	return Entry{
		Value: &value,
	}
}

// Forever creates a cache entry that shall never expire
func Forever(value interface{}) Entry {
	return Entry{
		Expiry: time.Unix(0, 0),
		Value:  &value,
	}
}

// NewStore instantiates a cache store given a Redis pool
func NewStore(p *redis.Pool) Store {
	gob.Register(map[string]string{})
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

	var value Entry
	valueBuffer := bytes.NewBuffer(valueBytes)
	dec := gob.NewDecoder(valueBuffer)
	err = dec.Decode(&value)
	if err != nil {
		return nil, err
	}

	return &value, nil
}

// GetOrDefer a cache entry, deferring to supplier func if entry doesn't exist
// or is expired. If key exists and is expired, entry is deleted
func (s *Store) GetOrDefer(key string, deferFn func() Entry) (*Entry, error) {
	exists, err := s.redisExists(key)
	if err != nil {
		return nil, err
	}
	if !exists {
		entry := deferFn()
		go s.Set(key, entry)
		return &entry, nil
	}

	entry, err := s.Get(key)
	if err != nil {
		return nil, err
	}

	if entry.IsExpired() {
		entry := deferFn()
		go s.Set(key, entry)
		return &entry, nil
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
