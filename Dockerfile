FROM microsoft/dotnet:2.2-aspnetcore-runtime

WORKDIR /app
COPY Server/bin/Release/netcoreapp2.2/publish/ .

CMD ["dotnet", "Server.dll"]
