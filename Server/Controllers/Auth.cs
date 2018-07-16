using System;
using System.Diagnostics;
using System.Linq;
using FlexLabs.EntityFrameworkCore.Upsert;
using JetBrains.Annotations;
using Models;
using Nancy;
using Newtonsoft.Json;
using Server.Configuration;
using Server.Services;
using Server.Services.Session;
using Skybrud.Social.Spotify.OAuth;
using Skybrud.Social.Spotify.Responses.Authentication;
using Skybrud.Social.Spotify.Scopes;
using Spotify.API.NetCore;
// ReSharper disable VirtualMemberCallInConstructor

namespace Server.Controllers
{
  [UsedImplicitly]
  public class Auth : NancyModule
  {
    private static readonly SpotifyScope[] PartySpotifyScopes = {
      SpotifyScopes.UserReadPrivate,
      SpotifyScopes.UserLibraryRead,
      SpotifyScopes.UserLibraryModify,
      SpotifyScopes.PlaylistReadPrivate,
      SpotifyScopes.PlaylistReadCollaborative
    };

    private readonly Session _session;
    private readonly PartyModelContainer _db;
    private readonly SpotifyOAuthClient _oAuth;

    public Auth(AppConfiguration appConfiguration, Session session, PartyModelContainer db)
    {
      _session = session;
      _db = db;
      _oAuth = new SpotifyOAuthClient(
        appConfiguration.Spotify.AppKey,
        appConfiguration.Spotify.AppSecret,
        appConfiguration.Spotify.Callback
       );

      ModulePath = "/auth";

      Get("/spotify", _ => LoginWithSpotify());
      Get(appConfiguration.Spotify.Callback, _ => SpotifyCallback());
      Get("/finished", _ => GetFinished());
    }

    private dynamic GetFinished()
    {
      return $"Logged in as {Context.CurrentUser.Identity.Name}";
    }

    private dynamic LoginWithSpotify()
    {
      var state = new Guid();
      _session.AuthState = state.ToString();

      var authUrl = _oAuth.GetAuthorizationUrl(state.ToString(), PartySpotifyScopes);

      return Response.AsRedirect(authUrl);
    }

    private dynamic SpotifyCallback()
    {
      var state = Request.Query["state"];
      if (_session.AuthState != state)
      {
        return Negotiate.WithStatusCode(HttpStatusCode.BadRequest);
      }

      _session.AuthState = null;

      var tokenResponse = _oAuth.GetAccessTokenFromAuthCode(Request.Query["code"]) as SpotifyTokenResponse;
      Debug.Assert(tokenResponse != null, nameof(tokenResponse) + " != null");
      var token = tokenResponse.Body;
      var tokenExpiryDate = DateTime.UtcNow.Add(token.ExpiresIn);

      var spotify = new SpotifyWebAPI()
      {
        UseAuth = true,
        UseAutoRetry = false,
        AccessToken = token.AccessToken,
        TokenType = token.TokenType
      };
      var spotifyUser = spotify.GetPrivateProfile();

      var user = new User()
      {
        AccessToken = token.AccessToken,
        TokenScope = PartySpotifyScopes.Aggregate("", (s, scope) => $"{s} {scope.Name}").Trim(),
        TokenExpiryDate = tokenExpiryDate,
        RefreshToken = token.RefreshToken,
        Username = spotifyUser.Id,
        SpotifyUser = JsonConvert.SerializeObject(spotifyUser, Formatting.None, new JsonSerializerSettings()
        {
          ContractResolver = Json.SnakeCaseContractResolver
        })
      };
      _db.Upsert(user).On(u => u.Username).Run();

      _session.Username = user.Username;

      return Response.AsRedirect("/auth/finished");
    }
  }
}
