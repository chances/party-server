using System;
using System.Collections;
using System.Linq;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Models;

namespace Server.Configuration
{
  public class AppConfiguration
  {
    private readonly IEnumerable<DictionaryEntry> _environmentVariables;

    public AppConfiguration()
    {
      Mode = Mode.Production;

      if (!File.Exists(".env"))
      {
        Console.WriteLine("Warning: .env file is not present. Using system provided environment variables");
      }
      else
      {
        Mode = Mode.Development;
      }

      LoadDotEnv();
      _environmentVariables = Environment.GetEnvironmentVariables().Cast<DictionaryEntry>();

      var modeString = GetVariable("MODE");
      if (modeString != null && modeString.ToLower() == "development")
      {
        Mode = Mode.Development;
      }

      var portString = GetVariable("PORT");
      if (portString != null && int.TryParse(portString, out var port))
      {
        Port = port;
      }
      else
      {
        Port = 3005;
      }

      Cors = new Cors(GetVariable("CORS_ORIGINS", "https://chancesnow.me"));

      var databaseUrl = GetVariable("DATABASE_URL");
      DatabaseUrl = databaseUrl ?? throw new NullReferenceException("Database URL configuration is missing");

      var redisUrl = GetVariable("REDIS_URL");
      RedisUrl = redisUrl ?? throw new NullReferenceException("Redis URL configuration is missing");

      var spotifyAppKey = GetVariable("SPOTIFY_APP_KEY");
      var spotifyAppSecret = GetVariable("SPOTIFY_APP_SECRET");
      var spotifyCallback = GetVariable("SPOTIFY_CALLBACK");

      if (spotifyAppKey == null || spotifyAppSecret == null || spotifyCallback == null)
      {
        throw new NullReferenceException("One or more Spotify configuration options is missing");
      }

      Spotify = new Spotify(spotifyAppKey, spotifyAppSecret, spotifyCallback);
    }

    public Mode Mode { get; }

    public int Port { get; }

    public Cors Cors { get; }

    public string DatabaseUrl { get; }
    public string ConnectionString => DatabaseUrl.ToPgsqlConnectionString();

    public string RedisUrl { get; }

    public Spotify Spotify { get; }

    private string GetVariable(string name, string defaultValue = null) =>
      (string) (_environmentVariables.FirstOrDefault(entry => (string) entry.Key == name).Value ?? defaultValue);

    private static void LoadDotEnv(string filePath = ".env", Encoding encoding = null)
    {
      if (!File.Exists(filePath))
      {
        return;
      }

      if (encoding == null)
      {
        encoding = Encoding.Default;
      }

      // read all lines from the env file
      var dotEnvContents = File.ReadAllText(filePath, encoding);

      // split the long string into an array of rows
      var dotEnvRows =
        dotEnvContents.Split(new[] {"\n", "\r\n", Environment.NewLine}, StringSplitOptions.RemoveEmptyEntries);

      // loop through rows, split into key and value then add to enviroment
      foreach (var dotEnvRow in dotEnvRows)
      {
        var index = dotEnvRow.IndexOf("=", StringComparison.Ordinal);

        if (index < 0) continue;
        var key = dotEnvRow.Substring(0, index).Trim();
        var value = dotEnvRow.Substring(index + 1, dotEnvRow.Length - (index + 1)).Trim();

        if (key.Length <= 0) continue;
        Environment.SetEnvironmentVariable(key, value.Length == 0 ? null : value);
      }
    }
  }

  public struct Cors
  {
    private readonly string _origins;

    public Cors(string origins)
    {
      _origins = origins;
    }

    public IEnumerable<string> AllowedOrigins =>
      _origins?.Split(",", StringSplitOptions.RemoveEmptyEntries) ?? new string[0];
  }

  public struct Spotify
  {
    public Spotify(string appKey, string appSecret, string callback)
    {
      AppKey = appKey;
      AppSecret = appSecret;
      Callback = callback;
    }

    public string AppKey { get; }

    public string AppSecret { get; }

    public string Callback { get; }
  }
}
