using Microsoft.AspNetCore.Http;
using Server.Services.Authentication;
using Spotify.API.NetCore.Models;

namespace Server.Services
{
  public class ProfileProvider
  {
    private readonly IHttpContextAccessor _context;

    public ProfileProvider(IHttpContextAccessor context)
    {
      _context = context;
    }

    public PrivateProfile Profile
    {
      get
      {
        var claims = _context.HttpContext?.User?.Claims ?? null;
        if (claims == null) return null;

        return SpotifyAuthenticationScheme.GetProfile(claims);
      }
    }
  }
}
