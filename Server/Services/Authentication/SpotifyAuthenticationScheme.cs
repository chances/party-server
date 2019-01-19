using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Claims;
using System.Threading.Tasks;
using LiteGuard;
using Microsoft.AspNetCore.Authentication.OAuth;
using Microsoft.EntityFrameworkCore;
using Models;
using Newtonsoft.Json;
using Spotify.API.NetCore;
using Spotify.API.NetCore.Enums;
using Spotify.API.NetCore.Models;

namespace Server.Services.Authentication
{
  public static class SpotifyAuthenticationScheme
  {
    public static readonly string Name = "Spotify";

    private static readonly string SpotifyAccessTokenClaim = "urn:party:spotify:accessToken";
    private static readonly string SpotifyRefreshTokenClaim = "urn:party:spotify:refreshToken";
    private static readonly string SpotifyTokenExpiryClaim = "urn:party:spotify:tokenExpiry";
    private static readonly string SpotifyUserJsonClaim = "urn:party:spotify:userJson";

    private static readonly Scope Scope =
      Scope.UserReadPrivate |
      Scope.PlaylistReadPrivate |
      Scope.PlaylistReadCollaborative;

    public static void Configure(OAuthOptions options, string appKey, string appSecret, string callback)
    {
      options.ClientId = appKey;
      options.ClientSecret = appSecret;
      options.CallbackPath = callback;
      options.AuthorizationEndpoint = "https://accounts.spotify.com/authorize";
      options.TokenEndpoint = "https://accounts.spotify.com/api/token";
      options.UserInformationEndpoint = "https://api.spotify.com/v1/me";
      options.SaveTokens = true;

      foreach (var scope in Scope.GetStringAttribute(",").Split(","))
      {
        options.Scope.Add(scope);
      }

      options.Events = new OAuthEvents
      {
        OnCreatingTicket = async context =>
        {
          var tokenExpiry = DateTime.UtcNow.AddSeconds(double.Parse(context.TokenResponse.ExpiresIn));

          var request = new HttpRequestMessage(HttpMethod.Get, context.Options.UserInformationEndpoint);
          request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
          request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", context.AccessToken);

          var response = await context.Backchannel.SendAsync(request, HttpCompletionOption.ResponseHeadersRead, context.HttpContext.RequestAborted);
          response.EnsureSuccessStatusCode();

          var userJson = await response.Content.ReadAsStringAsync();
          var spotifyUser = JsonConvert.DeserializeObject<PrivateProfile>(userJson);

          var claims = new List<Claim>
              {
                new Claim(ClaimsIdentity.DefaultRoleClaimType, Roles.Host),
                new Claim(ClaimTypes.NameIdentifier, spotifyUser.Id),
                new Claim(SpotifyAccessTokenClaim, context.AccessToken),
                new Claim(SpotifyRefreshTokenClaim, context.RefreshToken),
                new Claim(SpotifyTokenExpiryClaim, tokenExpiry.ToString()),
                new Claim(SpotifyUserJsonClaim, userJson)
              };
          context.Principal.AddIdentity(new ClaimsIdentity(claims, Name));
        }
      };
    }

    public static async Task UpsertPartyUser(PartyModelContainer dbContext, IEnumerable<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var userJson = claims[SpotifyUserJsonClaim].Value;
      var accessToken = claims[SpotifyAccessTokenClaim].Value;
      var refreshToken = claims[SpotifyRefreshTokenClaim].Value;
      var tokenExpiry = claims[SpotifyTokenExpiryClaim].Value;

      // TODO: Switch to SpotifyApi.NetCore? It's more often maintained, but has less API coverage
      var spotifyUser = JsonConvert.DeserializeObject<PrivateProfile>(userJson);

      var user = await dbContext.User
        .Where(u => u.Username == spotifyUser.Id)
        .DefaultIfEmpty(new User()
        {
          Username = spotifyUser.Id,
        })
        .FirstOrDefaultAsync();

      if (dbContext.Entry(user).State == EntityState.Detached)
      {
        dbContext.User.Add(user);
      }
      else
      {
        user.UpdatedAt = DateTime.UtcNow;
      }

      user.AccessToken = accessToken;
      user.RefreshToken = refreshToken;
      user.SpotifyUser = userJson;
      user.TokenExpiryDate = DateTime.Parse(tokenExpiry).ToUniversalTime();

      await dbContext.SaveChangesAsync();
    }

    /// <summary>
    /// Refreshs the Spotify access token associated with the given principal claims.
    /// </summary>
    /// <returns>Returns true if the access token is expired.</returns>
    /// <param name="principalClaims">Principal Claims.</param>
    public static async Task<bool> RefreshAccessToken(IEnumerable<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var userJson = claims[SpotifyUserJsonClaim].Value;
      var accessToken = claims[SpotifyAccessTokenClaim].Value;
      var refreshToken = claims[SpotifyRefreshTokenClaim].Value;
      var tokenExpiry = DateTime.Parse(claims[SpotifyTokenExpiryClaim].Value).ToUniversalTime();

      // TODO: Try to refresh Spotify access token
      var tokenExpired = IsTokenExpiryExpired(tokenExpiry);

      return tokenExpired;
    }

    public static PrivateProfile GetProfile(IEnumerable<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var userJson = claims[SpotifyUserJsonClaim]?.Value ?? null;

      if (userJson == null) return null;

      var spotifyUser = JsonConvert.DeserializeObject<PrivateProfile>(userJson);
      return spotifyUser;
    }

    public static Token GetToken(IEnumerable<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var accessToken = claims[SpotifyAccessTokenClaim]?.Value ?? null;

      if (accessToken == null) return null;

      var refreshToken = claims[SpotifyRefreshTokenClaim].Value;
      var tokenExpiry = DateTime.Parse(claims[SpotifyTokenExpiryClaim].Value).ToUniversalTime();

      if (IsTokenExpiryExpired(tokenExpiry)) return null;

      var expiresIn = (int) tokenExpiry.Subtract(DateTime.UtcNow).TotalSeconds;

      return new Token()
      {
        AccessToken = accessToken,
        RefreshToken = refreshToken,
        TokenType = "Bearer",
        ExpiresIn = expiresIn
      };
    }

    private static bool IsTokenExpiryExpired(DateTime tokenExpiry)
    {
      return tokenExpiry <= DateTime.UtcNow;
    }
  }
}
