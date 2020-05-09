#############
# Build stage
#############

FROM mcr.microsoft.com/dotnet/core/sdk:3.1-alpine AS build
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

FROM mcr.microsoft.com/dotnet/core/aspnet:3.1-alpine
WORKDIR /app
COPY --from=build /app .

CMD ["./Server"]
