package main

import (
	"database/sql"
	"log"
	"strings"
	"time"

	"github.com/fatih/camelcase"
	_ "github.com/lib/pq"
)

var (
	db *sql.DB
)

func initDatabase() *sql.DB {
	driver := "postgres"
	connString := getenvOrFatal("DATABASE_URL")

	newDb, err := sql.Open(driver, connString)
	if err != nil {
		log.Fatalf("Could not init database: %s\n", err)
	}

	newDb.SetMaxIdleConns(3)
	newDb.SetMaxOpenConns(10)

	err = newDb.Ping()
	if err != nil {
		log.Fatalf("Could not ping database: %s\n", err)
	}

	return newDb
}

type model struct {
	ID        int64
	CreatedAt time.Time `orm:"created_at"`
	UpdatedAt time.Time
}

func snakeCase(src string) string {
	words := camelcase.Split(src)

	// Map to lowercase words
	lowercaseWords := make([]string, len(words))
	for i, v := range words {
		lowercaseWords[i] = strings.ToLower(v)
	}

	return strings.Join(words, "_")
}
