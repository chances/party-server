using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Models;

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

    public async Task<Party> GetCurrentPartyAsync(PartyModelContainer db)
    {
      if (!_userProvider.IsAuthenticated) return null;

      if (_userProvider.IsUserGuest)
      {
        var guest = _userProvider.Guest;
        if (guest == null) return null;

        return await db.Party.FirstOrDefaultAsync(p => p.Id == guest.PartyId);
      }

      var user = await _userProvider.GetUserAsync(db);
      if (user == null) return null;
      await db.Entry(user).Reference(u => u.Party).LoadAsync();

      return user.Party;
    }
  }
}
