using System;
using System.Diagnostics;
using JetBrains.Annotations;
using Nancy;
using Server.Configuration;
using Server.Services.Repositories;
using Server.Services.Session;
using Skybrud.Social.Spotify.OAuth;
using Skybrud.Social.Spotify.Responses.Authentication;
using Skybrud.Social.Spotify.Scopes;
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
    private readonly IUserRepository _userRepository;
    private readonly SpotifyOAuthClient _oAuth;

    public Auth(AppConfiguration appConfiguration, Session session, IUserRepository userRepository)
    {
      _session = session;
      _userRepository = userRepository;
      _oAuth = new SpotifyOAuthClient(
        appConfiguration.Spotify.AppKey,
        appConfiguration.Spotify.AppSecret,
        appConfiguration.Spotify.Callback
       );

      ModulePath = "/auth";

      Get("/spotify", _ => LoginWithSpotify());
      Get("/spotify/callback", _ => SpotifyCallback());
      Get("/finished", _ => GetFinished());
    }

    private dynamic GetFinished()
    {
      return $"Logged in as {Context.CurrentUser.Identity.Name}";
    }

    private dynamic LoginWithSpotify()
    {
      var state = Guid.NewGuid();
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

      var user = _userRepository.CreateUserFromSpotify(token, PartySpotifyScopes);

      _session.Username = user.Username;

      return Response.AsRedirect("/auth/finished");
    }
  }
}
