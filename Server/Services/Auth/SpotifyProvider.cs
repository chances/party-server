using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using LiteGuard;
using RestSharp;
using Server.Services.Auth.Spotify;
using Server.Services.Spotify;
using SimpleAuthentication.Core;
using SimpleAuthentication.Core.Exceptions;
using SimpleAuthentication.Core.Providers;
using SimpleAuthentication.Core.Tracing;
using Spotify.API.NetCore;

namespace Server.Services.Auth
{
  public class SpotifyProvider : BaseOAuth20Provider<AccessTokenResult>
  {
    public static string Name = "Spotify";
    public static readonly string[] PartySpotifyScopes = {
      "user-read-private",
      "user-library-read",
      "user-library-modify",
      "playlist-read-private",
      "playlist-read-collaborative"
    };

    private const string AccessTokenKey = "access_token";
    private const string TokenTypeKey = "token_type";

    public SpotifyProvider(ProviderParams providerParams) : this(Name, providerParams) {}

    protected SpotifyProvider(string name, ProviderParams providerParams) : base(name, providerParams)
    {
      AuthenticateRedirectionUrl = new Uri("https://accounts.spotify.com/authorize");
    }

    public override IEnumerable<string> DefaultScopes => PartySpotifyScopes;

    public override string ScopeSeparator => " ";

    protected override IRestResponse<AccessTokenResult> ExecuteRetrieveAccessToken(string authorizationCode, Uri redirectUri)
    {
      Guard.AgainstNullArgument(nameof(authorizationCode), authorizationCode);

      if (redirectUri == null || string.IsNullOrEmpty(redirectUri.AbsoluteUri))
      {
        throw new ArgumentNullException(nameof(redirectUri));
      }

      var restRequest = new RestRequest("/api/token", Method.POST);
      restRequest.AddParameter("client_id", PublicApiKey);
      restRequest.AddParameter("client_secret", SecretApiKey);
      restRequest.AddParameter("redirect_uri", redirectUri.AbsoluteUri);
      restRequest.AddParameter("code", authorizationCode);
      restRequest.AddParameter("grant_type", "authorization_code");

      var restClient = RestClientFactory.CreateRestClient("https://accounts.spotify.com");
      TraceSource.TraceVerbose("Retrieving Access Token endpoint: {0}",
        restClient.BuildUri(restRequest).AbsoluteUri);

      return restClient.Execute<AccessTokenResult>(restRequest);
    }

    protected override AccessToken MapAccessTokenResultToAccessToken(AccessTokenResult accessTokenResult)
    {
      Guard.AgainstNullArgument(nameof(accessTokenResult), accessTokenResult);

      if (string.IsNullOrEmpty(accessTokenResult.AccessToken) ||
          string.IsNullOrEmpty(accessTokenResult.TokenType))
      {
        var errorMessage =
          string.Format(
            "Retrieved a Spotify Access Token but it doesn't contain one or more of either: {0} or {1}.",
            AccessTokenKey, TokenTypeKey);
        TraceSource.TraceError(errorMessage);
        throw new AuthenticationException(errorMessage);
      }

      return accessTokenResult;
    }

    protected override UserInformation RetrieveUserInformation(AccessToken accessToken)
    {
      Guard.AgainstNullArgument(nameof(accessToken), accessToken);
      if (!(accessToken is AccessTokenResult))
      {
        throw new ArgumentException(
          $"{nameof(accessToken)} is not an instance of Server.Services.Auth.Spotify.AccessTokenResult",
          nameof(accessToken));
      }
      if (string.IsNullOrEmpty(accessToken.PublicToken))
      {
        throw new ArgumentException("accessToken.PublicToken");
      }

      var token = (AccessTokenResult) accessToken;

      var spotify = new SpotifyWebAPI()
      {
        UseAuth = true,
        UseAutoRetry = false,
        AccessToken = token.PublicToken,
        TokenType = token.TokenType
      };
      var user = spotify.GetPrivateProfile();

      return new SpotifyUserInformation()
      {
        Id = user.Id,
        UserName = user.Id,
        Name = user.DisplayName,
        Email = user.Email ?? "",
        Picture = user.GetLargestImage().Url,
        Profile = user
      };
    }
  }
}
