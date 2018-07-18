using System;
using System.Linq;
using FlexLabs.EntityFrameworkCore.Upsert;
using JetBrains.Annotations;
using Models;
using Newtonsoft.Json;
using Skybrud.Social.Spotify.Objects.Authentication;
using Skybrud.Social.Spotify.Scopes;
using Spotify.API.NetCore;

namespace Server.Services.Repositories
{
  [UsedImplicitly]
  public class UserRepository : IUserRepository
  {
    private readonly PartyModelContainer _db;

    public UserRepository(PartyModelContainer db)
    {
      _db = db;
    }

    public User CreateUserFromSpotify(SpotifyToken token, SpotifyScope[] scopes)
    {
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
        TokenScope = scopes.Aggregate("", (s, scope) => $"{s} {scope.Name}").Trim(),
        TokenExpiryDate = tokenExpiryDate,
        RefreshToken = token.RefreshToken,
        Username = spotifyUser.Id,
        SpotifyUser = JsonConvert.SerializeObject(spotifyUser, Formatting.None, new JsonSerializerSettings()
        {
          ContractResolver = Json.SnakeCaseContractResolver
        })
      };
      _db.Upsert(user).On(u => u.Username).Run();

      return user;
    }

    public User GetUserByUsername(string username) => _db.User.FirstOrDefault(u => u.Username == username);
  }
}
