using System;
using LiteGuard;
using Microsoft.Extensions.Caching.Distributed;
using Microsoft.Extensions.Caching.Redis;

namespace Server.Services.Cache
{
  public class Cache
  {
    private readonly RedisCache _store;

    public Cache(RedisCache store)
    {
      _store = store;
    }

    public bool Exists(string key) => _store.Get(key) != null;

    public Entry<T> Get<T>(string key, Func<string, T> deserialize) where T : ISerializable
    {
      Guard.AgainstNullArgument(nameof(key), key);
      return Entry<T>.Forever(deserialize(_store.GetString(key)));
    }

    public Entry<T> GetOrDefer<T>(string key, Func<string, T> deserialize, Func<Entry<T>> defer) where T : ISerializable
    {
      if (Exists(key)) return Get(key, deserialize);

      var entry = defer();
      Set(key, entry);
      return entry;
    }

    public void Set<T>(string key, Entry<T> entry) where T : ISerializable
    {
      Guard.AgainstNullArgument(nameof(key), key);
      _store.SetString(key, entry.Value.Serialize(), entry.CacheOptions);
    }
  }
}
