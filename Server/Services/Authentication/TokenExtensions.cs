using System;
using System.Linq;
using System.Security.Claims;
using Spotify.API.NetCore.Models;

namespace Server.Services.Authentication
{
  public static class TokenExtensions
  {
    public static Token ToSpotifyToken(this ILookup<string, Claim> claims)
    {
      try
      {
        var accessTokenClaim = claims[SpotifyAuthenticationScheme.SpotifyAccessTokenClaim];
        var accessToken = accessTokenClaim.FirstOrDefault()?.Value;
        var refreshTokenClaim = claims[SpotifyAuthenticationScheme.SpotifyRefreshTokenClaim];
        var refreshToken = refreshTokenClaim.FirstOrDefault()?.Value;
        var claimTypes = claims.SelectMany(claimsByType => claimsByType.Select(claim => claim.Type)).ToHashSet();
        var tokenExpiresIn = int.Parse(
          claimTypes.Contains(SpotifyAuthenticationScheme.SpotifyTokenExpiryClaim)
            ? claims[SpotifyAuthenticationScheme.SpotifyTokenExpiryClaim].First()?.Value
            : "0"
        );
        var tokenCreationDate = DateTime.UtcNow.AddSeconds(-5);

        return new Token
        {
          TokenType = "Bearer",
          AccessToken = accessToken,
          RefreshToken = refreshToken,
          ExpiresIn = tokenExpiresIn,
          CreateDate = tokenCreationDate
        };
      }
      catch (Exception ex)
      {
        throw new NullReferenceException(
          "Missing the principal claims required to convert to a Spotify access token", ex);
      }
    }
  }
}
