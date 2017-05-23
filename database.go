package main

import (
	"log"

	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq"
	"github.com/vattle/sqlboiler/boil"
)

var (
	db *sqlx.DB
)

func initDatabase() *sqlx.DB {
	driver := "postgres"
	connString := getenvOrFatal("DATABASE_URL")

	newDb, err := sqlx.Connect(driver, connString)
	if err != nil {
		log.Fatalf("Could not init and ping database: %s\n", err)
	}

	newDb.SetMaxIdleConns(3)
	newDb.SetMaxOpenConns(10)

	boil.SetDB(newDb)

	return newDb
}
