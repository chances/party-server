using System;
using Microsoft.Extensions.Caching.Distributed;

namespace Server.Services.Cache
{
  public class Entry<T> where T : ISerializable
  {
    private Entry(T value)
    {
      Value = value;
    }

    public static Entry<T> Expires(DateTime expiry, T value)
    {
      return new Entry<T>(value)
      {
        CacheOptions = new DistributedCacheEntryOptions().SetAbsoluteExpiration(expiry)
      };
    }

    public static Entry<T> ExpiresAfter(TimeSpan lifetime, T value)
    {
      return new Entry<T>(value)
      {
        CacheOptions = new DistributedCacheEntryOptions().SetAbsoluteExpiration(lifetime)
      };
    }

    public static Entry<T> Forever(T value)
    {
      return new Entry<T>(value);
    }

    public T Value { get; }

    public DistributedCacheEntryOptions CacheOptions { get; private set; }
  }
}
