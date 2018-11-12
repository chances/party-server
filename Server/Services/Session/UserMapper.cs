using System.Linq;
using System.Security.Claims;
using JetBrains.Annotations;
using Models;
using Server.Services.Auth;
using Server.Services.Auth.Spotify;
using Server.Services.Repositories;

namespace Server.Services.Session
{
  [UsedImplicitly]
  public class UserMapper : IUserMapper
  {
    private readonly Session _session;
    private readonly IUserRepository _userRepository;

    public UserMapper(Session session, IUserRepository userRepository)
    {
      _session = session;
      _userRepository = userRepository;
    }

    public ClaimsPrincipal GetUserFromSession()
    {
      var currentUsername = _session.Username;
      if (string.IsNullOrWhiteSpace(currentUsername)) return null;

      var existingUser = _userRepository.GetUserByUsername(currentUsername);
      return existingUser != null
        ? new ClaimsPrincipal(new SpotifyIdentity(existingUser))
        : new ClaimsPrincipal(new SpotifyIdentity(currentUsername));
    }
  }
}
