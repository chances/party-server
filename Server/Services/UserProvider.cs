using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.Distributed;
using Models;
using Server.Models;
using Server.Services.Authentication;
using Server.Services.Background;

namespace Server.Services
{
  public class UserProvider : ScopedService
  {
    private PartyModelContainer _db;
    private IDistributedCache _cache;
    private IBackgroundTaskQueue _background;

    public UserProvider(
      IHttpContextAccessor context,
      PartyModelContainer db,
      IDistributedCache cache,
      IBackgroundTaskQueue background
    ) : base(context)
    {
      _db = db;
      _cache = cache;
      _background = background;

      // TODO: Check for guest token and whatnot
    }

    public bool IsAuthenticated =>
      HttpContext.User.Identity.IsAuthenticated &&
      (IsUserHost || IsUserGuest);

    public bool IsUserHost => HttpContext.User.IsInRole(Roles.Host);

    public bool IsUserGuest => HttpContext.User.IsInRole(Roles.Guest);

    public async Task<User> GetUserAsync()
    {
      if (!IsAuthenticated) return null;

      // Get user, maybe from cache 
      var username = HttpContext.User.Username();
      return await _cache.GetOrDeferAsync(
        username,
        async () => {
          var user = await _db.User
            .Where(u => u.Username == username)
            .FirstOrDefaultAsync();
          if (user != null) CacheUser(user);
          return user;
        }
      );
    }

    public Guest Guest
    {
      get
      {
        if (!IsAuthenticated) return null;

        return null;
      }
    }

    public void CacheUser(User user)
    {
      _background.QueueTask(
        async token => await _cache.SetAsync(
          user.Username,
          user,
          new DistributedCacheEntryOptions
          {
            AbsoluteExpirationRelativeToNow = TimeSpan.FromDays(1)
          }
        )
      );
    }
  }
}
