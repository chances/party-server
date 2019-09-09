using System;
using System.Linq;
using System.Security.Claims;
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
    private readonly PartyModelContainer _db;
    private readonly IDistributedCache _cache;
    private readonly IBackgroundTaskQueue _background;

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
    }

    public bool IsAuthenticated =>
      HttpContext.User.Identity.IsAuthenticated &&
      (IsUserHost || IsUserGuest);

    public bool IsUserHost => HttpContext.User.IsInRole(Roles.Host);

    public bool IsUserGuest => HttpContext.User.IsInRole(Roles.Guest);

    public Task<User> GetUserAsync()
    {
      return GetUserAsync(_db);
    }

    /// <summary>
    /// Gets the user asynchronously, bypassing the cache.
    /// </summary>
    /// <remarks>
    /// A user retrieved when bypassing the cache will update the cached user.
    /// </remarks>
    /// <returns>The user retrieved from the database.</returns>
    /// <param name="db">Party database container instance.</param>
    public async Task<User> GetUserAsync(PartyModelContainer db)
    {
      if (!IsAuthenticated) return null;

      // Get user from the DB and update the cached user
      var username = HttpContext.User.Username();
      var user = await db.User
            .Where(u => u.Username == username)
            .FirstOrDefaultAsync();
      if (user != null) CacheUser(user);
      return user;
    }

    public static bool GetIsUserHost(ClaimsPrincipal user) =>
      user?.IsInRole(Roles.Host) ?? false;

    public static bool GetIsUserGuest(ClaimsPrincipal user) =>
      user?.IsInRole(Roles.Guest) ?? false;

    public static bool GetIsAuthenticated(ClaimsPrincipal user)
    {
      var isUserHost = GetIsUserHost(user);
      var isUserGuest = GetIsUserGuest(user);

      return (user?.Identity.IsAuthenticated ?? false) &&
             (isUserHost || isUserGuest);
    }

    public static async Task<User> GetUserAsync(
      ClaimsPrincipal principal,
      PartyModelContainer db,
      IBackgroundTaskQueue background = null,
      IDistributedCache cache = null
    )
    {
      // Get user from the DB
      var username = principal.Username();
      if (username == null) return null;
      var user = await db.User
        .Where(u => u.Username == username)
        .FirstOrDefaultAsync();
      if (user != null && background != null && cache != null)
        CacheUser(background, cache, user);
      return user;
    }

    public Guest Guest
    {
      get
      {
        if (!IsAuthenticated) return null;

        var claims = HttpContext.User?.Claims;
        return CookiesAuthenticationScheme.GetGuest(claims);
      }
    }

    public static Guest GetGuest(ClaimsPrincipal user) =>
      CookiesAuthenticationScheme.GetGuest(user?.Claims);

    private void CacheUser(User user)
    {
      CacheUser(_background, _cache, user);
    }

    private static void CacheUser(IBackgroundTaskQueue background, IDistributedCache cache, User user)
    {
      background.QueueTask(
        async token => await cache.SetAsync(
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
