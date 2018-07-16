using SimpleAuthentication.Core;
using Spotify.API.NetCore.Models;

namespace Server.Services.Auth.Spotify
{
  public class SpotifyUserInformation : UserInformation
  {
    public PrivateProfile Profile { get; set; }
  }
}
