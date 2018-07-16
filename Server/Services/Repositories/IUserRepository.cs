using Models;
using Skybrud.Social.Spotify.Objects.Authentication;
using Skybrud.Social.Spotify.Scopes;

namespace Server.Services.Repositories
{
  public interface IUserRepository
  {
    User CreateUserFromSpotify(SpotifyToken token, SpotifyScope[] scopes);
  }
}
