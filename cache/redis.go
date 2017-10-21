package cache

import (
	"fmt"

	"github.com/garyburd/redigo/redis"
  "time"
  "strings"
  "strconv"
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

func (s *Store) redisExpire(key string, lifetime time.Duration) error {
  c := s.pool.Get()
  defer c.Close()

  // Use PEXPIRE redis command when lifetime is less than 1 second
  var keyWasExpired bool
  if lifetime.Seconds() < 1.0 {
    lifetimeMilliseconds := lifetime.Nanoseconds() / int64(time.Millisecond)
    resp, err := redis.Bool(c.Do(
      "PEXPIRE", int(lifetimeMilliseconds),
    ))
    if err != nil {
      return fmt.Errorf(
        "Could not PEXPIRE key %s in %d: %v",
          key, int(lifetimeMilliseconds), err,
      )
    }
    keyWasExpired = resp
  } else {
    resp, err := redis.Bool(c.Do(
      "EXPIRE", int(lifetime.Seconds()),
      ))
    if err != nil {
      return fmt.Errorf(
        "Could not EXPIRE key %s in %d: %v",
          key, int(lifetime.Seconds()), err,
      )
    }
    keyWasExpired = resp
  }

  if !keyWasExpired {
    return fmt.Errorf("Key %s does not exist", key)
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

func (s *Store) redisDeleteKeys(keys []string) error {
	c := s.pool.Get()
	defer c.Close()

	for _, key := range keys {
		_, err := c.Do("DEL", key)
		if err != nil {
			return err
		}
	}

	return nil
}
