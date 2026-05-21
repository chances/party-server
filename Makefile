C_SHARP_SRC=Server
C_SHARP_SOURCES=$(shell find $(C_SHARP_SRC) -type f -name "*.cs")
CS_HTML_SOURCES=$(shell find $(C_SHARP_SRC) -type f -name "*.cshtml")

all: build
.DEFAULT_GOAL := build

build: $(C_SHARP_SOURCES) $(CS_HTML_SOURCES)
	dotnet build
.PHONY: build

.PHONY: run
run: build
	dotnet run --project Server

.PHONY: docker
docker:
	docker build -t party-server .

.PHONY: docker-test
docker-test:
	docker-compose up -d --renew-anon-volumes

.PHONY: publish
publish: docker
	docker tag party-server registry.heroku.com/chances-party-staging/web
	heroku container:push web -a chances-party-staging
	heroku container:release web -a chances-party-staging

.PHONY: clean
clean:
	dotnet clean
	rm -rf Server/bin Server/obj
