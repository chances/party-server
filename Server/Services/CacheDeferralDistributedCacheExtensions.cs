using System;
using System.Threading.Tasks;
using Microsoft.Extensions.Caching.Distributed;
using Newtonsoft.Json;

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

    public static T GetOrDefer<T>(this IDistributedCache cache, string key, Func<T> defer)
    {
      return GetOrDefer(cache, key, JsonConvert.DeserializeObject<T>, defer);
    }

    public static T GetOrDefer<T>(this IDistributedCache cache, string key, Func<string, T> convert, Func<T> defer)
    {
      var cachedValue = cache.GetString(key);
      return cachedValue != null ? convert(cachedValue) : defer();
    }

    public static async Task<T> GetOrDeferAsync<T>(this IDistributedCache cache, string key, Func<Task<T>> defer)
    {
      return await GetOrDeferAsync(cache, key, JsonConvert.DeserializeObject<T>, defer);
    }

    public static async Task<T> GetOrDeferAsync<T>(this IDistributedCache cache, string key, Func<string, T> convert, Func<Task<T>> defer)
    {
      var cachedValue = await cache.GetStringAsync(key);
      return cachedValue != null ? convert(cachedValue) : await defer();
    }

    public static void Set(this IDistributedCache cache, string key, object value, DistributedCacheEntryOptions options = null)
    {
      cache.Set(key, value, JsonConvert.SerializeObject, options);
    }

    public static void Set<T>(this IDistributedCache cache, string key, T value, Func<T, string> convert, DistributedCacheEntryOptions options = null)
    {
      cache.SetString(key, convert(value), options);
    }

    public static async Task SetAsync(this IDistributedCache cache, string key, object value, DistributedCacheEntryOptions options = null)
    {
      await cache.SetAsync(key, value, JsonConvert.SerializeObject, options);
    }

    public static async Task SetAsync<T>(this IDistributedCache cache, string key, T value, Func<T, string> convert, DistributedCacheEntryOptions options = null)
    {
      await cache.SetStringAsync(key, convert(value), options);
    }
  }
}
