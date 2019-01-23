using System;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;

namespace Server.Services.Background
{
  public class QueuedHostedService : BackgroundService
  {
    private readonly ILogger _logger;

    public QueuedHostedService(IBackgroundTaskQueue taskQueue,
      ILoggerFactory loggerFactory)
    {
      TaskQueue = taskQueue;
      _logger = loggerFactory.CreateLogger<QueuedHostedService>();
    }

    private IBackgroundTaskQueue TaskQueue { get; }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
      _logger.LogInformation("Queued Hosted Service is starting.");

      while (!stoppingToken.IsCancellationRequested)
      {
        var workItem = await TaskQueue.DequeueAsync(stoppingToken);

        try
        {
          await workItem(stoppingToken);
        }
        catch (Exception ex)
        {
          _logger.LogError(ex,
             $"Error occurred executing {nameof(workItem)}.");
        }
      }

      _logger.LogInformation("Queued Hosted Service is stopping.");
    }
  }
}
