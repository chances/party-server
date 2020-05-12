using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Claims;
using System.Threading.Tasks;
using LiteGuard;
using Microsoft.EntityFrameworkCore;
using Models;
using Newtonsoft.Json;
using Server.Services.Authorization;
using Spotify.API.NetCore;
using Spotify.API.NetCore.Enums;
using Spotify.API.NetCore.Models;

namespace Server.Services.Authentication
{
  public static class SpotifyAuthenticationScheme
  {
    public const string Name = "Spotify";

    public const string SpotifyAccessTokenClaim = "https://apps.chancesnow.me/auth/spotify/access_token";
    public const string SpotifyRefreshTokenClaim = "https://apps.chancesnow.me/auth/spotify/refresh_token";
    public const string SpotifyTokenExpiryClaim = "https://apps.chancesnow.me/auth/spotify/expires_in";
    public const string SpotifyTokenScopeClaim = "https://apps.chancesnow.me/auth/spotify/scope";

    private static readonly Scope RequiredScopes =
      Scope.UserReadPrivate |
      Scope.UserLibraryRead |
      Scope.UserLibraryModify |
      Scope.PlaylistReadPrivate |
      Scope.PlaylistReadCollaborative;

    public static bool HasRequiredScopes(string spotifyTokenScope)
    {
      var requiredScopes = RequiredScopes.GetStringAttribute(" ").Split(' ');
      return spotifyTokenScope.Split(' ').All(scope => requiredScopes.Contains(scope));
    }

    public static async Task UpsertPartyUser(
      PartyModelContainer dbContext,
      Token spotifyToken,
      string spotifyTokenScope,
      PrivateProfile spotifyProfile)
    {
      Guard.AgainstNullArgument(nameof(spotifyToken), spotifyToken);
      Guard.AgainstNullArgument(nameof(spotifyTokenScope), spotifyTokenScope);
      Guard.AgainstNullArgument(nameof(spotifyProfile), spotifyProfile);

      var tokenExpiry = spotifyToken.CreateDate.AddSeconds(spotifyToken.ExpiresIn - 5);

      var userJson = JsonConvert.SerializeObject(spotifyProfile);
      var user = await dbContext.User
        .Where(u => u.Username == spotifyProfile.Id)
        .FirstOrDefaultAsync();

      if (user == null)
      {
        user = new User()
        {
          Username = spotifyProfile.Id
        };
        dbContext.User.Add(user);
      }
      else
      {
        user.UpdatedAt = DateTime.UtcNow;
      }

      user.AccessToken = spotifyToken.AccessToken;
      user.RefreshToken = spotifyToken.RefreshToken;
      user.TokenExpiryDate = tokenExpiry;
      user.TokenScope = spotifyTokenScope;
      user.SpotifyUser = userJson;

      await dbContext.SaveChangesAsync();
    }

    /// <summary>
    /// Refreshes the Spotify access token associated with the given principal claims.
    /// </summary>
    /// <returns>Returns a new <see cref="ClaimsPrincipal"/> if the access token was refreshed.</returns>
    /// <param name="principalClaims">Principal Claims.</param>
    /// <param name="config">App configuration for Spotify authentication.</param>
    public static async Task<ClaimsPrincipal> RefreshAccessToken(Token accessToken, string spotifyUserId, Configuration.Spotify config)
    {
      Guard.AgainstNullArgument(nameof(accessToken), accessToken);

      // Try to refresh the access token if expired, only once
      if (accessToken.IsExpired())
      {
        var refreshedToken = await TryRefreshToken(config.AppKey, config.AppSecret, accessToken.RefreshToken);
        if (refreshedToken != null)
        {
          var newClaims = new List<Claim>
          {
            new Claim(ClaimsIdentity.DefaultRoleClaimType, Roles.Host),
            new Claim(ClaimTypes.NameIdentifier, spotifyUserId),
            new Claim(SpotifyAccessTokenClaim, refreshedToken.AccessToken),
            new Claim(SpotifyRefreshTokenClaim, refreshedToken.RefreshToken),
            new Claim(SpotifyTokenExpiryClaim, refreshedToken.ExpiresIn.ToString())
          };
          return new ClaimsPrincipal(new ClaimsIdentity(newClaims, Name));
        }
      }

      return null;
    }

    private static async Task<Token> TryRefreshToken(string appKey, string appSecret, string refreshToken)
    {
      using (var client = new HttpClient())
      {
        try
        {
          var refreshRequestData = new List<KeyValuePair<string, string>>
          {
            new KeyValuePair<string, string>("grant_type", "refresh_token"),
            new KeyValuePair<string, string>("refresh_token", refreshToken)
          };
          var request = new HttpRequestMessage(HttpMethod.Post, "https://accounts.spotify.com/api/token")
          {
            Content = new FormUrlEncodedContent(refreshRequestData)
          };
          request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
          var authorization = Base64Encode($"{appKey}:{appSecret}");
          request.Headers.Authorization = new AuthenticationHeaderValue("Basic", authorization);

          var response = await client.SendAsync(request);
          response.EnsureSuccessStatusCode();

          string json = await response.Content.ReadAsStringAsync();
          return JsonConvert.DeserializeObject<Token>(json);
        }
        catch (HttpRequestException)
        {
          // TODO: Report this to Sentry
          return null;
        }
      }
    }

    private static string Base64Encode(string plainText)
    {
      var plainTextBytes = System.Text.Encoding.UTF8.GetBytes(plainText);
      return Convert.ToBase64String(plainTextBytes);
    }

    public static Token GetToken(List<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToLookup(claim => claim.Type);
      return claims.ToSpotifyToken();
    }
  }
}
