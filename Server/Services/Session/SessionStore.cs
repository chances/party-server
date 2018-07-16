using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using JetBrains.Annotations;
using Microsoft.Extensions.Caching.Distributed;
using Microsoft.Extensions.Caching.Redis;

namespace Server.Services.Session
{
  [UsedImplicitly]
  public class SessionStore
  {
    private readonly RedisCache _store;

    public SessionStore(RedisCache store)
    {
      _store = store;
    }

    public string Get(string key) => _store.GetString(key);

    public void Set(string key, string value) => _store.SetString(key, value);

    public void Delete(string key) => _store.Remove(key);

    public void SaveFlashes(Guid sessionId, Dictionary<string, string> flashes)
    {
      var storedFlashes = flashes.Aggregate("",
        delegate(string s, KeyValuePair<string, string> pair)
        {
          var value = pair.Value.Replace("'", "&QUOT;");
          return $"{s},{pair.Key}:'{value}'";
        });
      _store.SetString($"{sessionId}:flashes", storedFlashes);
    }

    public Dictionary<string, string> LoadFlashes(Guid sessionId)
    {
      var flashes = new Dictionary<string, string>();
      var loadedFlashes = _store.GetString($"{sessionId}:flashes");
      if (loadedFlashes == null) return flashes;

      foreach (var pair in loadedFlashes.Split(',', StringSplitOptions.RemoveEmptyEntries))
      {
        var keyAndValue = pair.Split(':');
        Debug.Assert(keyAndValue.Length == 2);
        var value = keyAndValue[1].Replace("&QUOT;", "'");
        flashes.Add(keyAndValue[0], value);
      }

      return flashes;
    }
  }
}
