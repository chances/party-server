using System.Linq;
using System.Security.Claims;
using JetBrains.Annotations;
using Models;
using Server.Services.Auth;
using Server.Services.Auth.Spotify;

namespace Server.Services.Session
{
  [UsedImplicitly]
  public class UserMapper : IUserMapper
  {
    private readonly Session _session;
    private readonly PartyModelContainer _db;

    public UserMapper(Session session, PartyModelContainer db)
    {
      _session = session;
      _db = db;
    }

    public ClaimsPrincipal GetUserFromSession()
    {
      var currentUsername = _session.Username;
      if (string.IsNullOrWhiteSpace(currentUsername)) return null;

      var existingUser = _db.User.FirstOrDefault(u => u.Username == currentUsername);
      return existingUser != null
        ? new ClaimsPrincipal(new SpotifyIdentity(existingUser))
        : new ClaimsPrincipal(new SpotifyIdentity(currentUsername));
    }
  }
}
