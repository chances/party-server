package cache

import (
	"fmt"

	"github.com/garyburd/redigo/redis"
)

func (s *Store) redisGet(key string) (string, error) {
	c := s.pool.Get()
	defer c.Close()

	value, err := redis.String(c.Do("GET", key))
	if err != nil {
		return "", fmt.Errorf("Could not GET %s: %v", key, err)
	}

	return value, nil
}

func (s *Store) redisGetBytes(key string) ([]byte, error) {
	c := s.pool.Get()
	defer c.Close()

	value, err := redis.Bytes(c.Do("GET", key))
	if err != nil {
		return nil, fmt.Errorf("Could not GET %s: %v", key, err)
	}

	return value, nil
}

func (s *Store) redisSet(key string, value interface{}) error {
	c := s.pool.Get()
	defer c.Close()

	// status, err
	_, err := c.Do("SET", key, value)
	if err != nil {
		return fmt.Errorf("Could not SET key %s to %s: %v", key, value, err)
	}

	return nil
}

func (s *Store) redisExists(key string) (bool, error) {
	c := s.pool.Get()
	defer c.Close()

	ok, err := redis.Bool(c.Do("EXISTS", key))
	if err != nil {
		return ok, fmt.Errorf("Could not determine if key %s exists: %v", key, err)
	}
	return ok, err
}

func (s *Store) redisDelete(key string) error {
	c := s.pool.Get()
	defer c.Close()

	_, err := c.Do("DEL", key)
	return err
}
