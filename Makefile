C_SHARP_SRC=Server
C_SHARP_SOURCES=$(shell find $(C_SHARP_SRC) -type f -name "*.cs")
CS_HTML_SOURCES=$(shell find $(C_SHARP_SRC) -type f -name "*.cshtml")
SERVER_TARGET=Server/bin/Release/netcoreapp2.1/publish/Server.dll
DOCKERFILE_TARGET=Server/bin/Release/netcoreapp2.1/publish/Dockerfile

all: build
.DEFAULT_GOAL := build

build: $(C_SHARP_SOURCES) $(CS_HTML_SOURCES)
	dotnet build
.PHONY: build

$(DOCKERFILE_TARGET): $(C_SHARP_SOURCES) $(CS_HTML_SOURCES)
	dotnet publish -c Release
	cp Dockerfile Server/bin/Release/netcoreapp2.1/publish
	docker build -t party-server Server/bin/Release/netcoreapp2.1/publish

.env.docker: .env.example
	cp .env.example .env.docker

docker: $(DOCKERFILE_TARGET)
.PHONY: docker

docker-test: $(DOCKERFILE_TARGET) .env.docker
	docker run --name party-server -p 3005:3005 --rm -d --env-file ./.env.docker party-server
.PHONY: docker-test

publish: $(DOCKERFILE_TARGET)
	docker tag party-server registry.heroku.com/chances-party-staging/web
	heroku container:push web -a chances-party-staging
	heroku container:release web -a chances-party-staging
.PHONY: publish

clean:
	rm -rf Server/bin/Release
.PHONY: clean
