SOURCES := $(shell find . -name '*.go')

chances-party: $(SOURCES)
	go get -v ./...
	go build
