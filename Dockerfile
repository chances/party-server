FROM microsoft/dotnet:2.1-aspnetcore-runtime

WORKDIR /app
COPY . .

CMD ["dotnet", "Server.dll"]
