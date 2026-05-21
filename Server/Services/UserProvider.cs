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
using Server.Services.Authorization;
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
      (HttpContext.User?.Identity.IsAuthenticated ?? false) && (IsUserHost || IsUserGuest);
    public bool IsUserHost => HttpContext.User?.IsInRole(Roles.Host) ?? false;
    public bool IsUserGuest => HttpContext.User?.IsInRole(Roles.Guest) ?? false;

    /// <summary>
    /// Gets the user asynchronously, optionally bypassing the cache.
    /// </summary>
    /// <remarks>
    /// A user retrieved when bypassing the cache will update the cached user.
    /// </remarks>
    /// <returns>The retrieved user.</returns>
    /// <param name="bypassCache">Whether or not to bypass the cache.</param>
    public Task<User> GetUserAsync(bool bypassCache = false)
    {
      return GetUserAsync(_db, bypassCache);
    }

    private Task<User> GetUserAsync(PartyModelContainer db, bool bypassCache = false)
    {
      if (!IsAuthenticated) return null;

      var username = HttpContext.User.Identity.Name;
      return GetUserAsync(username, db, _cache, _background);
    }

    /// <summary>
    /// Gets the user asynchronously, optionally bypassing the cache.
    /// </summary>
    /// <remarks>
    /// A user retrieved given a <paramref name="cache"/> and <paramref name="bg"/> will update the cached user.
    /// </remarks>
    /// <returns>The retrieved user.</returns>
    /// <param name="username"></param>
    /// <param name="db"></param>
    /// <param name="cache">If provided with <paramref name="bg"/>, try the cache first.</param>
    /// <param name="bg">If provided with <paramref name="cache"/>, try the cache first.</param>
    public static Task<User> GetUserAsync(
      string username,
      PartyModelContainer db,
      IDistributedCache cache = null,
      IBackgroundTaskQueue background = null
    )
    {
      if (username == null) return null;
      // Get user from the DB and update the cached user
      Func<Task<User>> getUser = async () =>
      {
          var user = await db.User
              .Where(u => u.Username == username)
              .FirstOrDefaultAsync();
        if (user != null && background != null && cache != null)
        {
          CacheUser(background, cache, user);
        }
        return user;
      };
      return cache == null ? getUser() : cache.GetOrDeferAsync(username, getUser);
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
