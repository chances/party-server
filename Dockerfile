#############
# Build stage
#############

# Learn about building .NET container images:
# https://github.com/dotnet/dotnet-docker/blob/main/samples/README.md
FROM --platform=$BUILDPLATFORM mcr.microsoft.com/dotnet/sdk:8.0 AS build
ARG TARGET_ARCH
WORKDIR /src

COPY . "/src"
# Delete .env files. Use `docker run --env_file .env` or the env_file key in docker-compose.yml
RUN rm -f .env docker.env
RUN dotnet restore
RUN dotnet publish -c Release -o /app --no-restore
# Copy static assets
COPY Server/public /app/public

###################
# Application stage
###################

FROM mcr.microsoft.com/dotnet/runtime:8.0-alpine
WORKDIR /app
COPY --from=build /app .

CMD ["./Server"]
