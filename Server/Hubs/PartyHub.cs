using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Microsoft.AspNetCore.SignalR;
using Microsoft.Extensions.Caching.Distributed;
using Models;
using Server.Models;
using Server.Services;
using Server.Services.Background;
using Server.Services.Channels;

namespace Server.Hubs
{
  [UsedImplicitly]
  public class PartyHub : Hub<IPartyHubClient>
  {
    private readonly PartyModelContainer _db;
    private readonly IBackgroundTaskQueue _background;
    private readonly IDistributedCache _cache;
    private readonly EventChannel<PublicParty> _partyChannel;

    public PartyHub(
      PartyModelContainer db,
      IBackgroundTaskQueue background,
      IDistributedCache cache,
      EventChannel<PublicParty> partyChannel
    )
    {
      _db = db;
      _background = background;
      _cache = cache;
      _partyChannel = partyChannel;
    }

    public override async Task OnConnectedAsync()
    {
      var httpContext = Context.GetHttpContext();
      if (httpContext == null)
      {
        await Clients.Caller.BadRequest();
        return;
      }

      var currentParty = await PartyProvider.GetCurrentPartyAsync(
        httpContext.User, _db, _background, _cache);
      if (currentParty == null)
      {
        await Clients.Caller.BadRequest("You haven't joined a party yet.");
        return;
      }

      await Groups.AddToGroupAsync(Context.ConnectionId, currentParty.RoomCode);
    }

    [UsedImplicitly]
    public async Task<ChannelReader<PublicParty>> Party(CancellationToken token)
    {
      var currentParty = await PartyProvider.GetCurrentPartyAsync(
        Context.User, _db, _background, _cache);
      if (currentParty == null)
      {
        _ = Clients.Caller.BadRequest("You haven't joined a party yet.");

        var channel = Channel.CreateBounded<PublicParty>(0);
        channel.Writer.Complete();
        return channel.Reader;
      }

      var roomCode = currentParty.RoomCode;
      return await _partyChannel.Listen(party => party.RoomCode == roomCode, token);
    }
  }

  public interface IPartyHubClient
  {
    Task BadRequest(string message = "");
  }
}
