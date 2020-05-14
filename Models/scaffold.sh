#!/usr/bin/env bash

# Scaffold the Entity Framework DbContext
#
# Run this IN the Models project folder!

dotnet ef dbcontext scaffold -p ./Models.csproj -s ./../ModelsScaffold/ModelsScaffold.csproj -f "Host=localhost;Database=party;Username=party;Password=party" Npgsql.EntityFrameworkCore.PostgreSQL -d -c PartyModelContainer --schema public
