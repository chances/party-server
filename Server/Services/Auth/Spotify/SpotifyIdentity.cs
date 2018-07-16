using System.Security.Principal;
using Models;
using Newtonsoft.Json;
using Spotify.API.NetCore.Models;

namespace Server.Services.Auth.Spotify
{
  public class SpotifyIdentity : IIdentity
  {
    public SpotifyIdentity(User user)
    {
      User = user;
      Profile = JsonConvert.DeserializeObject<PrivateProfile>(user.SpotifyUser);
      IsAuthenticated = true;

      Name = Profile?.DisplayName ?? User?.Username ?? "Spotify User";
    }

    public SpotifyIdentity(string username)
    {
      IsAuthenticated = false;
      Name = username;
    }

    public User User { get; }
    public PrivateProfile Profile { get; }

    public string AuthenticationType => "OAuth";
    public bool IsAuthenticated { get; }
    public string Name { get; }
  }
}
