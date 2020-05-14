using System;
using Newtonsoft.Json;

namespace Server.Models
{
  public class SpotifyToken
  {
    public SpotifyToken(string accessToken, DateTime tokenExpiry)
    {
      AccessToken = accessToken;
      TokenExpiry = tokenExpiry;
    }

    [JsonProperty("access_token")]
    public string AccessToken { get; set; }

    [JsonProperty("token_expiry")]
    public DateTime TokenExpiry { get; set; }
  }
}
