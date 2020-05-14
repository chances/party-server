using System;
using System.Reactive.Linq;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;

namespace Server.Services.Channels
{
  public class EventChannel<T> : IEventChannel<T>
  {
    private readonly Mutex _updatesMutex;
    private readonly IObservable<T> _entityUpdates;

    protected delegate void UpdatedHandler(T e);
    protected event UpdatedHandler EntityUpdated;

    public EventChannel()
    {
      _updatesMutex = new Mutex(false, GetType().Name);
      _entityUpdates = Observable.FromEvent<UpdatedHandler, T>(
        handler => e => handler(e),
        handler => EntityUpdated += handler,
        handler => EntityUpdated -= handler
      );
    }

    public void Push(T eventValue)
    {
      EntityUpdated?.Invoke(eventValue);
    }

    public async Task<ChannelReader<T>> Listen(Predicate<T> eventFilter, CancellationToken token)
    {
      return await Task.Run(() =>
      {
        var channel = Channel.CreateUnbounded<T>();

        _updatesMutex.WaitOne();

        _entityUpdates.Subscribe(entity =>
          {
            channel.Writer.WriteAsync(entity, token);
          },
          (ex) => { channel.Writer.TryComplete(ex); },
          () => { channel.Writer.TryComplete(); },
          token
        );

        _updatesMutex.ReleaseMutex();

        return channel.Reader;
      }, token);
    }

    public void Dispose()
    {
      _updatesMutex.Dispose();
    }
  }
}
