using System;
using System.Reactive.Linq;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using Server.Models;

namespace Server.Services.Channels
{
  public interface IEventChannel<T> : IDisposable
  {
    /// <summary>
    /// Push a new value event to subscribed clients.
    /// </summary>
    /// <param name="eventValue">New event value</param>
    void Push(T eventValue);

    Task<ChannelReader<T>> Listen(Predicate<T> eventFilter, CancellationToken token);
  }

  public class PartyChannel : IEventChannel<PublicParty>
  {
    private readonly Mutex _partyUpdatesMutex;
    private readonly IObservable<PublicParty> _partyUpdates;

    protected delegate void PartyUpdatedHandler(PublicParty e);
    protected event PartyUpdatedHandler PartyUpdated;

    public PartyChannel()
    {
      _partyUpdatesMutex = new Mutex(false, GetType().Name);
      _partyUpdates = Observable.FromEvent<PartyUpdatedHandler, PublicParty>(
        handler => e => handler(e),
        handler => PartyUpdated += handler,
        handler => PartyUpdated -= handler
      );
    }

    public void Push(PublicParty party)
    {
      PartyUpdated?.Invoke(party);
    }

    public async Task<ChannelReader<PublicParty>> Listen(Predicate<PublicParty> eventFilter, CancellationToken token)
    {
      return await Task.Run(() =>
      {
        var channel = Channel.CreateUnbounded<PublicParty>();

        _partyUpdatesMutex.WaitOne();

        _partyUpdates.Subscribe(party =>
          {
            channel.Writer.WriteAsync(party, token);
          },
          (ex) => { channel.Writer.TryComplete(ex); },
          () => { channel.Writer.TryComplete(); },
          token
        );

        _partyUpdatesMutex.ReleaseMutex();

        return channel.Reader;
      }, token);
    }

    public void Dispose()
    {
      _partyUpdatesMutex.Dispose();
    }
  }
}
