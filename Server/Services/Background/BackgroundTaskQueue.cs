using System;
using System.Collections.Concurrent;
using System.Threading;
using System.Threading.Tasks;
using LiteGuard;

namespace Server.Services.Background
{
  public interface IBackgroundTaskQueue
  {
    void QueueTask(Func<CancellationToken, Task> workItem);

    Task<Func<CancellationToken, Task>> DequeueAsync(
        CancellationToken cancellationToken);
  }

  public class BackgroundTaskQueue : IBackgroundTaskQueue
  {
    private ConcurrentQueue<Func<CancellationToken, Task>> _workItems =
        new ConcurrentQueue<Func<CancellationToken, Task>>();
    private SemaphoreSlim _signal = new SemaphoreSlim(0);

    public void QueueTask(Func<CancellationToken, Task> workItem)
    {
      Guard.AgainstNullArgument(nameof(workItem), workItem);

      _workItems.Enqueue(workItem);
      _signal.Release();
    }

    public async Task<Func<CancellationToken, Task>> DequeueAsync(CancellationToken cancellationToken)
    {
      await _signal.WaitAsync(cancellationToken);
      _workItems.TryDequeue(out var workItem);

      return workItem;
    }
  }
}
