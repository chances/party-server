using System;
using System.Globalization;
using Newtonsoft.Json;

namespace Server.Models
{
  public class SpotifyToken
  {
    public SpotifyToken(string accessToken, DateTime tokenExpiry)
    {
      AccessToken = accessToken;
      TokenExpiry = tokenExpiry.ToString(CultureInfo.InvariantCulture);
    }

    [JsonProperty("access_token")]
    public string AccessToken { get; set; }

    [JsonProperty("token_expiry")]
    public string TokenExpiry { get; set; }
  }
}
