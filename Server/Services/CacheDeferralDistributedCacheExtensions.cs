using System;
using System.Threading.Tasks;
using Microsoft.Extensions.Caching.Distributed;

namespace Server.Services
{
  public static class CacheDeferralDistributedCacheExtensions
  {
    public static string GetStringOrDefer(this IDistributedCache cache, string key, Func<string> defer)
    {
      return cache.GetString(key) ?? defer();
    }

    public static async Task<string> GetStringOrDeferAsync(this IDistributedCache cache, string key, Func<Task<string>> defer)
    {
      var cachedValue = await cache.GetStringAsync(key);
      return cachedValue ?? await defer();
    }

    public static async Task<T> GetOrDeferAsync<T>(this IDistributedCache cache, string key, Func<string, T> convert, Func<Task<T>> defer)
    {
      var cachedValue = await cache.GetStringAsync(key);
      return cachedValue != null ? convert(cachedValue) : await defer();
    }
  }
}
