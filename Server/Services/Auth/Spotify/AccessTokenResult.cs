using System;
using SimpleAuthentication.Core;

namespace Server.Services.Auth.Spotify
{
  public class AccessTokenResult : AccessToken
  {
    public string AccessToken {
      get => PublicToken;
      set => PublicToken = value;
    }
    public string TokenType { get; set; }
    public string Scope { get; set; }
    public int ExpiresIn {
      get => ExpiresOn.Subtract(DateTime.UtcNow).Seconds;
      set => DateTime.UtcNow.AddSeconds(value);
    }
    public string RefreshToken { get; set; }
  }
}
