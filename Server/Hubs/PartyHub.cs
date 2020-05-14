using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Microsoft.AspNetCore.SignalR;
using Microsoft.Extensions.Caching.Distributed;
using Models;
using Server.Data;
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
    private readonly EventChannel<Resource<Queue>> _queueChannel;
    private readonly EventChannel<Resource<History>> _historyChannel;

    public PartyHub(
      PartyModelContainer db,
      IBackgroundTaskQueue background,
      IDistributedCache cache,
      EventChannel<PublicParty> partyChannel,
      EventChannel<Resource<Queue>> queueChannel,
      EventChannel<Resource<History>> historyChannel
    )
    {
      _db = db;
      _background = background;
      _cache = cache;
      _partyChannel = partyChannel;
      _queueChannel = queueChannel;
      _historyChannel = historyChannel;
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
        httpContext.User, _db, _cache, _background);
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
        Context.User, _db, _cache, _background);
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

    [UsedImplicitly]
    public async Task<ChannelReader<Resource<Queue>>> Queue(CancellationToken token)
    {
      var currentParty = await PartyProvider.GetCurrentPartyAsync(
        Context.User, _db, _cache, _background);
      if (currentParty == null)
      {
        _ = Clients.Caller.BadRequest("You haven't joined a party yet.");

        var channel = Channel.CreateBounded<Resource<Queue>>(0);
        channel.Writer.Complete();
        return channel.Reader;
      }

      var roomCode = currentParty.RoomCode;
      return await _queueChannel.Listen(queue => queue.Id == roomCode, token);
    }

    [UsedImplicitly]
    public async Task<ChannelReader<Resource<History>>> History(CancellationToken token)
    {
      var currentParty = await PartyProvider.GetCurrentPartyAsync(
        Context.User, _db, _cache, _background);
      if (currentParty == null)
      {
        _ = Clients.Caller.BadRequest("You haven't joined a party yet.");

        var channel = Channel.CreateBounded<Resource<History>>(0);
        channel.Writer.Complete();
        return channel.Reader;
      }

      var roomCode = currentParty.RoomCode;
      return await _historyChannel.Listen(history => history.Id == roomCode, token);
    }
  }

  public interface IPartyHubClient
  {
    Task BadRequest(string message = "");
  }
}
