using System.Linq;
using Microsoft.AspNetCore.Http;
using Server.Services.Authentication;

namespace Server.Services
{
  public class ProfileProvider : ScopedService
  {
    public ProfileProvider(IHttpContextAccessor context) : base(context)
    {
      Profile = null;

      if (!HttpContext?.User?.Identity.IsAuthenticated ?? true) return;

      var claims = HttpContext?.User?.Claims;
      if (claims != null)
      {
        Profile = SpotifyAuthenticationScheme.GetProfile(claims.ToList());
      }
    }

    public Models.Spotify.PrivateProfile Profile { get; }
  }
}
