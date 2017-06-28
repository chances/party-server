SOURCES := $(shell find . -name '*.go')

all: chances-party

chances-party: $(SOURCES)
	go get -v ./...
	go build

models:
	sqlboiler --wipe --no-hooks postgres

.PHONY: models
