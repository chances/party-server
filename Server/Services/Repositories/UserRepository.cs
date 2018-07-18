using System;
using System.Linq;
using JetBrains.Annotations;
using Microsoft.EntityFrameworkCore;
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
      var existingUser = _db.User
        .Where(u => u.Username == user.Username)
        .Select(u => new {u.Id, u.CreatedAt})
        .FirstOrDefault();
      if (existingUser != null)
      {
        user.Id = existingUser.Id;
        user.CreatedAt = existingUser.CreatedAt;
        user.UpdatedAt = DateTime.UtcNow;
        _db.User.Attach(user).State = EntityState.Modified;
      }
      else
      {
        _db.User.Add(user);
      }
      _db.SaveChanges();

      return user;
    }

    public User GetUserByUsername(string username) => _db.User.FirstOrDefault(u => u.Username == username);
  }
}
