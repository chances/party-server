SOURCES := $(shell find . -name '*.go')

all: party-server

party-server: $(SOURCES)
	go get -v ./...
	go build

models:
	sqlboiler --wipe --no-hooks postgres

.PHONY: models
