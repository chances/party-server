using System;
using System.Linq;
using System.Web;

namespace Models
{
  public static class ConnectionStringExtensions
  {
    public static string ToPgsqlConnectionString(this string source, bool isDevelopmentEnv)
    {
      var isUrl = Uri.TryCreate(source, UriKind.Absolute, out var url);
      if (!isUrl) throw new ArgumentException($"Invalid PgSQL connection string: {source}", nameof(source));
      var port = url.Port == -1 ? 5432 : url.Port;
      var queryParams = HttpUtility.ParseQueryString(url.Query);
      var requireSsl = queryParams["ssl"] != null && queryParams["ssl"] == "true";
      var disableSsl = queryParams["ssl"] != null && queryParams["ssl"] == "false";
      var connectionString =
        $"Host={url.Host};Port={port};Database={url.LocalPath.Substring(1)};Username={url.UserInfo.Split(':')[0]};Password={url.UserInfo.Split(':')[1]};SSL Mode=Prefer;Pooling=true;";
      if (requireSsl)
      {
        connectionString = connectionString.Replace("SSL Mode=Prefer", "SSL Mode=Require");
      }
      if (disableSsl)
      {
        connectionString = connectionString.Replace("SSL Mode=Prefer", "SSL Mode=Disable");
      }

      if (!isDevelopmentEnv)
      {
        connectionString += ";Trust Server Certificate=true";
      }

      return connectionString;
    }
  }
}
