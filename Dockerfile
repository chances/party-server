
# FROM microsoft/aspnetcore:1
# LABEL Name=party-server Version=0.0.1
# ARG source=.
# WORKDIR /app
# EXPOSE 3005
# COPY $source .
# ENTRYPOINT dotnet party-server.dll

FROM microsoft/dotnet:2.1-sdk-alpine AS build
WORKDIR /app

# copy csproj and restore as distinct layers
COPY Server/*.csproj ./Server/
COPY Models/*.csproj ./Models/
WORKDIR /app/Server
RUN dotnet restore

# copy and publish app and libraries
WORKDIR /app/
COPY Server/. ./Server/
COPY Models/. ./Models/
WORKDIR /app/Server
RUN dotnet publish -c Release -o out


# test application -- see: dotnet-docker-unit-testing.md
# TODO: Add unit tets
# FROM build AS testrunner
# WORKDIR /app/Tests
# COPY Tests/. .
# ENTRYPOINT ["dotnet", "test", "--logger:trx"]


FROM microsoft/dotnet:2.1-runtime-alpine AS runtime
WORKDIR /app
COPY --from=build /app/Server/out ./
# ENV ASPNETCORE_URLS=http://+:${PORT}
# CMD dotnet Server.dll --server.urls=http://0.0.0.0:$PORT
CMD ["dotnet", "Server.dll"]
ENTRYPOINT ["dotnet", "Server.dll"]
