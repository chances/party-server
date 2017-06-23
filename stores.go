package main

import (
  "database/sql"
	"log"
	"time"

	"github.com/chances/chances-party/cache"
	"github.com/garyburd/redigo/redis"
	_ "github.com/lib/pq"
	"github.com/vattle/sqlboiler/boil"
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

func initDatabase() *sql.DB {
	connString := getenvOrFatal("DATABASE_URL")

	newDb, err := sql.Open("postgres", connString)
	if err != nil {
		log.Fatalf("Could not connect to database: %s\n", err)
	}
  err = newDb.Ping()
  if err != nil {
    log.Fatalf("Could not ping database: %s\n", err)
  }

	newDb.SetMaxIdleConns(3)
	newDb.SetMaxOpenConns(10)

	boil.SetDB(newDb)
	boil.SetLocation(time.UTC)

	return newDb
}
