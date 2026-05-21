using System;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Models;
using Server.Models;
using Server.Services.Channels;

namespace Server.Services.Jobs
{
  [UsedImplicitly]
  public class PruneExpiredGuestsService : IHostedService, IDisposable
  {
    private readonly IEventChannel<PublicParty> _partyChannel;
    private readonly ILogger _logger;
    private Timer _timer;

    public PruneExpiredGuestsService(
      IServiceProvider services,
      IEventChannel<PublicParty> partyChannel,
      ILogger<PruneExpiredGuestsService> logger
    )
    {
      Services = services;
      _partyChannel = partyChannel;
      _logger = logger;
    }

    private IServiceProvider Services { get; }

    public Task StartAsync(CancellationToken cancellationToken)
    {
      _logger.LogInformation("Expired Guests Pruning Service is starting.");

      // PruneExpiredGuests from active parties every ten seconds
      _timer = new Timer(PruneExpiredGuests, null, TimeSpan.Zero,
        TimeSpan.FromSeconds(10));

      return Task.CompletedTask;
    }

    private void PruneExpiredGuests(object state)
    {
      var prunedGuests = 0;

      using (var scope = Services.CreateScope())
      {
        var db = scope.ServiceProvider.GetRequiredService<PartyModelContainer>();
        // Get all parties that haven't ended
        var ongoingParties = db.Party.Where(p => p.Ended == false).ToList();
        foreach (var party in ongoingParties)
        {
          db.Entry(party).Reference(p => p.Guests).Load();
          var guests = party.GuestList();
          var numGuests = guests.Count;

          // Remove expired guests
          var expiredGuests = guests.Where(g => g.IsExpired).ToList();
          var numRemovedGuests = guests.RemoveAll(g => expiredGuests.Contains(g));
          prunedGuests += numRemovedGuests;

          // If the guest list changed then update the party's guest list and
          //  broadcast changed party
          if (numGuests == guests.Count) continue;
          party.UpdateGuestList(guests);
          db.SaveChanges();

          _partyChannel.Push(PublicParty.FromParty(party, guests));
        }
      }

      if (prunedGuests > 0)
      {
        _logger.LogInformation($"Pruned {prunedGuests} expired guests.");
      }
    }

    public Task StopAsync(CancellationToken cancellationToken)
    {
      _logger.LogInformation("Expired Guests Pruning Service is stopping.");

      _timer?.Change(Timeout.Infinite, 0);

      return Task.CompletedTask;
    }

    public void Dispose()
    {
      _timer?.Dispose();
    }
  }
}
