using System;
using System.Net.Http;
using FlexLabs.EntityFrameworkCore.Upsert;
using Microsoft.EntityFrameworkCore.Internal;
using Models;
using Nancy;
using Nancy.SimpleAuthentication;
using Newtonsoft.Json;
using Server.Services.Auth.Spotify;

namespace Server.Services.Auth
{
  public class SpotifyCallbackProvider : IAuthenticationCallbackProvider
  {
    private DbContextPool<PartyModelContainer> _dbPool;

    public SpotifyCallbackProvider(DbContextPool<PartyModelContainer> dbPool)
    {
      _dbPool = dbPool;
    }

    public dynamic Process(NancyModule nancyModule, AuthenticateCallbackData model)
    {
      if (model.ProviderName == SpotifyProvider.Name)
      {
        if (!(model.AuthenticatedClient.AccessToken is AccessTokenResult))
        {
          throw new ArgumentException(
            "Access token is not an instance of Server.Services.Auth.Spotify.AccessTokenResult",
            // ReSharper disable once NotResolvedInText
            "model.AuthenticatedClient.AccessToken");
        }
        if (!(model.AuthenticatedClient.UserInformation is SpotifyUserInformation))
        {
          throw new ArgumentException(
            "User information is not an instance of Server.Services.Auth.Spotify.SpotifyUserInformation",
            // ReSharper disable once NotResolvedInText
            "model.AuthenticatedClient.UserInformation");
        }

        var token = (AccessTokenResult) model.AuthenticatedClient.AccessToken;
        var user = (SpotifyUserInformation) model.AuthenticatedClient.UserInformation;

        var db = _dbPool.Rent();
        db.Upsert(new User()
        {
          AccessToken = token.AccessToken,
          RefreshToken = token.RefreshToken,
          SpotifyUser = JsonConvert.SerializeObject(user.Profile, Formatting.None, new JsonSerializerSettings()
          {
            ContractResolver = Json.SnakeCaseContractResolver
          })
        }).On(u => u.SpotifyUser).Run();
        _dbPool.Return(db);

        // TODO: Set user in context
        // TODO: Set user in session

        return nancyModule.Response.AsRedirect("/auth/finished");
      }

      return nancyModule.Negotiate.WithStatusCode(HttpStatusCode.BadRequest);
    }

    public dynamic OnRedirectToAuthenticationProviderError(NancyModule nancyModule, string errorMessage)
    {
      var context = nancyModule.Context;
      // TODO: Get more details from the spotify auth error

      throw new HttpRequestException(errorMessage)
      {
        Source = "SpotifyAuth"
      };
    }
  }
}
