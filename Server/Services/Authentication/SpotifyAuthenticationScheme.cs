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
    private static readonly string SpotifyUserJsonClaim = "urn:party:spotify:userJson";

    private static readonly Scope Scope =
      Scope.UserReadPrivate |
      Scope.UserLibraryRead |
      Scope.UserLibraryModify |
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
          var token = new Token()
          {
            AccessToken = context.AccessToken,
            TokenType = context.TokenType,
            RefreshToken = context.RefreshToken,
            CreateDate = DateTime.UtcNow,
            ExpiresIn = (int) context.ExpiresIn.GetValueOrDefault(TimeSpan.FromSeconds(0)).TotalSeconds
          };

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
                new Claim(SpotifyAccessTokenClaim, JsonConvert.SerializeObject(token)),
                new Claim(SpotifyUserJsonClaim, userJson)
              };
          context.Principal.AddIdentity(new ClaimsIdentity(claims, Name));
        }
      };
    }

    public static async Task UpsertPartyUser(PartyModelContainer dbContext, List<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var userJson = claims[SpotifyUserJsonClaim].Value;
      var accessTokenJson = claims[SpotifyAccessTokenClaim].Value;
      var accessToken = JsonConvert.DeserializeObject<Token>(accessTokenJson);
      var tokenExpiry = accessToken.CreateDate.AddSeconds(accessToken.ExpiresIn);

      // TODO: Switch to SpotifyApi.NetCore? It's more often maintained, but has less API coverage
      var spotifyUser = JsonConvert.DeserializeObject<PrivateProfile>(userJson);

      var user = await dbContext.User
        .Where(u => u.Username == spotifyUser.Id)
        .FirstOrDefaultAsync();

      if (user == null)
      {
        user = new User()
        {
          Username = spotifyUser.Id
        };
        dbContext.User.Add(user);
      }
      else
      {
        user.UpdatedAt = DateTime.UtcNow;
      }

      user.AccessToken = accessToken.AccessToken;
      user.RefreshToken = accessToken.RefreshToken;
      user.TokenExpiryDate = accessToken.CreateDate.AddSeconds(accessToken.ExpiresIn);
      user.TokenScope = Scope.GetStringAttribute(",");
      user.SpotifyUser = userJson;
      user.TokenExpiryDate = tokenExpiry.ToUniversalTime();

      await dbContext.SaveChangesAsync();
    }

    public static bool IsAccessTokenExpired(List<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var accessTokenJson = claims[SpotifyAccessTokenClaim].Value;
      var accessToken = JsonConvert.DeserializeObject<Token>(accessTokenJson);

      return accessToken.IsExpired();
    }

    /// <summary>
    /// Refreshes the Spotify access token associated with the given principal claims.
    /// </summary>
    /// <returns>Returns a new <see cref="ClaimsPrincipal"/> if the access token was refreshed.</returns>
    /// <param name="principalClaims">Principal Claims.</param>
    /// <param name="config">App configuration for Spotify authentication.</param>
    public static async Task<ClaimsPrincipal> RefreshAccessToken(List<Claim> principalClaims, Configuration.Spotify config)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var userJson = claims[SpotifyUserJsonClaim].Value;
      var accessTokenJson = claims[SpotifyAccessTokenClaim].Value;
      var accessToken = JsonConvert.DeserializeObject<Token>(accessTokenJson);

      // Try to refresh the access token if expired, only once
      if (accessToken.IsExpired())
      {
        var refreshedToken = await TryRefreshToken(config.AppKey, config.AppSecret, accessToken.RefreshToken);
        if (refreshedToken != null)
        {
          var userId = claims[ClaimTypes.NameIdentifier].Value;

          var newClaims = new List<Claim>
          {
            new Claim(ClaimsIdentity.DefaultRoleClaimType, Roles.Host),
            new Claim(ClaimTypes.NameIdentifier, userId),
            new Claim(SpotifyAccessTokenClaim, JsonConvert.SerializeObject(refreshedToken)),
            new Claim(SpotifyUserJsonClaim, userJson)
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
          return null;
        }
      }
    }

    private static string Base64Encode(string plainText)
    {
      var plainTextBytes = System.Text.Encoding.UTF8.GetBytes(plainText);
      return Convert.ToBase64String(plainTextBytes);
    }

    public static PrivateProfile GetProfile(List<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var userJson = claims[SpotifyUserJsonClaim]?.Value;

      if (userJson == null) return null;

      var spotifyUser = JsonConvert.DeserializeObject<PrivateProfile>(userJson);
      return spotifyUser;
    }

    public static Token GetToken(List<Claim> principalClaims)
    {
      Guard.AgainstNullArgument(nameof(principalClaims), principalClaims);

      var claims = principalClaims.ToDictionary(claim => claim.Type);
      var accessTokenJson = claims[SpotifyAccessTokenClaim]?.Value;

      if (accessTokenJson == null) return null;

      var accessToken = JsonConvert.DeserializeObject<Token>(accessTokenJson);

      return accessToken;
    }
  }
}
