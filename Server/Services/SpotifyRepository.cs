using System;
using System.Threading.Tasks;
using Spotify.API.NetCore;
using Spotify.API.NetCore.Models;
using Microsoft.AspNetCore.Http;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using Server.Services.Authentication;
using Microsoft.Extensions.Caching.Distributed;
using Newtonsoft.Json;
using Server.Models;
using Server.Services.Background;

namespace Server.Services
{
  public class SpotifyRepository : ScopedService
  {
    private static readonly IEnumerable<Playlist> EmptyList = new List<Playlist>(0);

    private readonly IBackgroundTaskQueue _background;
    private readonly IDistributedCache _cache;
    private readonly Token _spotifyAccessToken;

    public SpotifyRepository(
      IBackgroundTaskQueue queue,
      IDistributedCache cache,
      IHttpContextAccessor context
    ) : base(context)
    {
      _background = queue;
      _cache = cache;

      if (HttpContext.User.IsInRole(Roles.Host))
      {
        _spotifyAccessToken = SpotifyAuthenticationScheme.GetToken(HttpContext.User.Claims);
      }
    }

    public async Task<IEnumerable<Playlist>> GetMyOwnPlaylists()
    {
      var user = HttpContext.User;
      if (!user.IsInRole(Roles.Host))
      {
        return EmptyList;
      }

      var userId = user.Claims.First(c => c.Type == ClaimTypes.NameIdentifier).Value;

      return await GetMyPlaylists(true);
    }

    public async Task<IEnumerable<Playlist>> GetMyPlaylists(bool owned = false)
    {
      var user = HttpContext.User;
      if (!user.IsInRole(Roles.Host))
      {
        return EmptyList;
      }

      var userId = user.Claims.First(c => c.Type == ClaimTypes.NameIdentifier).Value;

      if (_spotifyAccessToken == null) return EmptyList;

      var playlists = await _cache.GetOrDeferAsync(
        $"playlists:{userId}",
        JsonConvert.DeserializeObject<List<Playlist>>,
        async () =>
        {
          var api = new SpotifyWebAPI()
          {
            UseAuth = true,
            AccessToken = _spotifyAccessToken.AccessToken,
            TokenType = _spotifyAccessToken.TokenType
          };

          var results = new List<Playlist>();

          Paging<SimplePlaylist> page = null;
          do
          {
            var offset = page != null
              ? page.Offset + page.Items.Count
              : 0;
            page = await api.GetUserPlaylistsAsync(userId, 50, offset);

            IEnumerable<SimplePlaylist> pageOfPlaylists = page.Items;

            if (owned)
            {
              pageOfPlaylists = pageOfPlaylists
                .Where(p => p.Owner.Id == userId);
            }

            results.AddRange(pageOfPlaylists.Select(Playlist.FromSpotify));
          } while (page.HasNextPage());

          return results;
        }
      );

      _background.QueueTask(async token =>
      {
        var json = JsonConvert.SerializeObject(playlists);
        await _cache.SetStringAsync($"playlists:{userId}", json, new DistributedCacheEntryOptions()
        {
          AbsoluteExpirationRelativeToNow = TimeSpan.FromHours(1)
        });
      });

      return playlists;
    }
  }
}
