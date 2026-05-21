using System;
using System.Linq;

namespace Server.Configuration
{
  public static class RedisConnectionStringExtensions
  {
    public static string ToRedisConnectionString(this string source)
    {
      var isUrl = Uri.TryCreate(source, UriKind.Absolute, out var url);
      if (!isUrl) throw new ArgumentException($"Invalid Redis connection string: {source}", nameof(source));
      var queryParams = url.Query.Split(';').ToDictionary(
        s => s.Contains('=') ? s.Split('=')[0].Trim() : s,
        s => s.Contains('=') ? s.Split('=')[1].Trim() : ""
      );
      var requireSsl = queryParams.ContainsKey("ssl") && queryParams["ssl"] == "true";
      var connectionString =
        $"{url.Host}:{url.Port}";
      if (url.UserInfo.Length > 0)
      {
        connectionString += $",password={url.UserInfo.Split(':')[1]}";
      }
      if (requireSsl)
      {
        connectionString = connectionString.Replace("SSL Mode=Prefer", "SSL Mode=Require");
      }

      return connectionString;
    }
  }
}
