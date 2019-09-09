using System;
using System.Linq;

namespace Models
{
  public static class ConnectionStringExtensions
  {
    public static string ToPgsqlConnectionString(this string source, bool isDevelopmentEnv)
    {
      var isUrl = Uri.TryCreate(source, UriKind.Absolute, out var url);
      if (!isUrl) throw new ArgumentException($"Invalid PgSQL connection string: {source}", nameof(source));
      var queryParams = url.Query.Split(';').ToDictionary(
        s => s.Contains('=') ? s.Split('=')[0].Trim() : s,
        s => s.Contains('=') ? s.Split('=')[1].Trim() : ""
      );
      var requireSsl = queryParams.ContainsKey("ssl") && queryParams["ssl"] == "true";
      var connectionString =
        $"Host={url.Host};Port={url.Port};Database={url.LocalPath.Substring(1)};Username={url.UserInfo.Split(':')[0]};Password={url.UserInfo.Split(':')[1]};SSL Mode=Prefer;Pooling=true;";
      if (requireSsl)
      {
        connectionString = connectionString.Replace("SSL Mode=Prefer", "SSL Mode=Require");
      }

      if (!isDevelopmentEnv)
      {
        connectionString += ";Trust Server Certificate=true";
      }

      return connectionString;
    }
  }
}
