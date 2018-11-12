using System;
using System.Linq;

namespace Server.Services.Cache
{
  public static class ConnectionStringExtensions
  {
    public static string ToRedisConnectionString(this string source)
    {
      var isUrl = Uri.TryCreate(source, UriKind.Absolute, out var url);
      if (!isUrl || url.Scheme != "redis")
        throw new ArgumentException($"Invalid Redis connection string: {source}", nameof(source));
      var queryParams = url.Query.Split(';').ToDictionary(
        s => s.Contains('=') ? s.Split('=')[0].Trim() : s,
        s => s.Contains('=') ? s.Split('=')[1].Trim() : ""
      );
      var host = string.IsNullOrWhiteSpace(url.Host) ? "localhost" : url.Host;
      var port = url.IsDefaultPort ? "6379" : url.Port.ToString();
      var requireSsl = queryParams.ContainsKey("ssl") && queryParams["ssl"] == "true";

      var connectionString = $"{host}:{port}";
      if (!string.IsNullOrWhiteSpace(url.UserInfo))
      {
        var userInfo = url.UserInfo.Split(':');
        connectionString = $"{connectionString},name={userInfo[0]},password={userInfo[1]}";
      }
      if (requireSsl)
      {
        connectionString = $"{connectionString},ssl=true";
      }

      return connectionString;
    }
  }
}
