using System;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;

namespace Server.Services.Channels
{
    public interface IEventChannel<T> : IDisposable
  {
    /// <summary>
    /// Push a new value event to subscribed clients.
    /// </summary>
    /// <param name="eventValue">New event value</param>
    void Push(T eventValue);
  }
}
