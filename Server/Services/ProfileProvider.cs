using Microsoft.AspNetCore.Http;
using Server.Services.Authentication;
using Spotify.API.NetCore.Models;

namespace Server.Services
{
  public class ProfileProvider : ScopedService
  {
    public ProfileProvider(IHttpContextAccessor context) : base(context)
    {
      Profile = null;

      if (!HttpContext?.User?.Identity.IsAuthenticated ?? true) return;

      var claims = HttpContext?.User?.Claims ?? null;
      if (claims != null)
      {
        Profile = SpotifyAuthenticationScheme.GetProfile(claims);
      }
    }

    public PrivateProfile Profile { get; }
  }
}
