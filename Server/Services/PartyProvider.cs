using System.Security.Claims;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.Distributed;
using Models;
using Server.Services.Authorization;
using Server.Services.Background;

namespace Server.Services
{
  public class PartyProvider : ScopedService
  {
    private readonly PartyModelContainer _db;
    private readonly UserProvider _userProvider;

    public PartyProvider(
      IHttpContextAccessor context,
      PartyModelContainer db,
      UserProvider userProvider
    ) : base(context)
    {
      _db = db;
      _userProvider = userProvider;
    }

    public async Task<Party> GetCurrentPartyAsync()
    {
      return await GetCurrentPartyAsync(_db);
    }

    private async Task<Party> GetCurrentPartyAsync(PartyModelContainer db)
    {
      if (!_userProvider.IsAuthenticated) return null;

      if (_userProvider.IsUserGuest)
      {
        var guest = _userProvider.Guest;
        if (guest == null) return null;

        return await db.Party.FirstOrDefaultAsync(p => p.Id == guest.PartyId);
      }

      var user = await _userProvider.GetUserAsync();
      if (user == null) return null;
      await db.Entry(user).Reference(u => u.Party).LoadAsync();

      return user.Party;
    }

    /// <summary>
    /// Gets the current <see cref="Party"/> asynchronously, optionally bypassing the cache to
    /// get the user.
    /// </summary>
    /// <remarks>
    /// A party's user retrieved given a <paramref name="cache"/> and <paramref name="background"/> will update the cached user.
    /// </remarks>
    /// <returns>The retrieved party.</returns>
    /// <param name="principal"></param>
    /// <param name="db"></param>
    /// <param name="cache">If provided with <paramref name="background"/>, try the cache first.</param>
    /// <param name="background">If provided with <paramref name="cache"/>, try the cache first.</param>
    public static async Task<Party> GetCurrentPartyAsync(
      ClaimsPrincipal principal,
      PartyModelContainer db,
      IDistributedCache cache = null,
      IBackgroundTaskQueue background = null
    )
    {
      if (!principal.Identity.IsAuthenticated) return null;

      if (!principal.IsInRole(Roles.Host))
      {
        var guest = UserProvider.GetGuest(principal);
        if (guest == null) return null;

        return await db.Party.FirstOrDefaultAsync(p => p.Id == guest.PartyId);
      }

      var username = principal.Identity.Name;
      var user = await UserProvider.GetUserAsync(username, db, cache, background);
      if (user == null) return null;
      await db.Entry(user).Reference(u => u.Party).LoadAsync();

      return user.Party;
    }
  }
}
