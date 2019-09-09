C_SHARP_SRC=Server
C_SHARP_SOURCES=$(shell find $(C_SHARP_SRC) -type f -name "*.cs")
CS_HTML_SOURCES=$(shell find $(C_SHARP_SRC) -type f -name "*.cshtml")
SERVER_TARGET=Server/bin/Release/netcoreapp2.2/publish/Server.dll

all: build
.DEFAULT_GOAL := build

build: $(C_SHARP_SOURCES) $(CS_HTML_SOURCES)
	dotnet build
.PHONY: build

run: build
	dotnet run --project Server
.PHONY: run

$(SERVER_TARGET): $(C_SHARP_SOURCES) $(CS_HTML_SOURCES)
	dotnet publish -c Release

docker: $(SERVER_TARGET)
	docker build -t party-server .
.PHONY: docker

docker-test: $(SERVER_TARGET) docker
	docker run --name party-server -p 3005:3005 --rm -d --env-file ./.env.docker party-server
.PHONY: docker-test

publish: docker
	docker tag party-server registry.heroku.com/chances-party-staging/web
	heroku container:push web -a chances-party-staging
	heroku container:release web -a chances-party-staging
.PHONY: publish

clean:
	rm -rf Server/bin/Release
.PHONY: clean
