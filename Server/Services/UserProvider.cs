using System.Linq;
using System.Security.Claims;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Caching.Distributed;
using Models;
using Server.Models;
using Server.Services.Authentication;

namespace Server.Services
{
  public class UserProvider : ScopedService
  {
    public UserProvider(
      IHttpContextAccessor context,
      PartyModelContainer db,
      IDistributedCache cache
    ) : base(context)
    {
      User = null;
      Guest = null;

      if (!HttpContext?.User?.Identity.IsAuthenticated ?? true) return;

      if (IsUserHost)
      {
        var username = HttpContext.User.Claims
        .Where(c => c.Type == ClaimTypes.NameIdentifier)
        .Select(c => c.Value)
        .FirstOrDefault();

        User = db.User.Where(u => u.Username == username).FirstOrDefault();
      }

      // TODO: Check for guest token and whatnot
    }

    public bool IsAuthenticated => User != null || Guest != null;

    public bool IsUserHost => HttpContext.User.IsInRole(Roles.Host);

    public bool IsUserGuest => HttpContext.User.IsInRole(Roles.Guest);

    public User User { get; }

    public Guest Guest { get; }
  }
}
