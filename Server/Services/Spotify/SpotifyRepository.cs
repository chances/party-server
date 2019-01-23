using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.Distributed;
using Models;
using Newtonsoft.Json;
using Server.Models;
using Server.Services.Authentication;
using Server.Services.Background;
using Spotify.API.NetCore;
using Spotify.API.NetCore.Models;

namespace Server.Services.Spotify
{
  public class SpotifyRepository : ScopedService
  {
    private static readonly IEnumerable<Playlist> EmptyList = new List<Playlist>(0);

    private readonly IBackgroundTaskQueue _background;
    private readonly PartyModelContainer _db;
    private readonly IDistributedCache _cache;
    private readonly SpotifyWebAPI _api;

    public SpotifyRepository(
      IBackgroundTaskQueue bgQueue,
      PartyModelContainer db,
      IDistributedCache cache,
      IHttpContextAccessor context
    ) : base(context)
    {
      _background = bgQueue;
      _db = db;
      _cache = cache;

      if (HttpContext.User.IsInRole(Roles.Host))
      {
        var token = SpotifyAuthenticationScheme.GetToken(HttpContext.User.Claims);
        if (token == null) return;

        _api = new SpotifyWebAPI()
        {
          UseAuth = true,
          AccessToken = token.AccessToken,
          TokenType = token.TokenType
        };
      }
    }

    // TODO: Rename these with async suffixes
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

      if (_api == null) return EmptyList;

      var playlists = await _cache.GetOrDeferAsync(
        $"playlists:{userId}",
        JsonConvert.DeserializeObject<List<Playlist>>,
        async () =>
        {
          var results = new List<Playlist>();

          Paging<SimplePlaylist> page = null;
          do
          {
            var offset = page != null
              ? page.Offset + page.Items.Count
              : 0;
            page = await _api.GetUserPlaylistsAsync(userId, 50, offset);

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

    public async Task<CachedPlaylist> GetPlaylistAsync(string userId, string playlistId)
    {
      // Try get the cached playlist from the cache, database, or via Spotify
      //  on cache & DB miss
      return await _cache.GetOrDeferAsync($"playlist:{playlistId}", async () =>
      {
        var trackList = await _db.TrackList
          .FirstOrDefaultAsync(t => t.SpotifyPlaylistId == playlistId);

        var oneHour = TimeSpan.FromHours(1);
        var oneDay = TimeSpan.FromHours(24);

        // Only cache playlists in the DB for one day
        var dbCacheValidWindow = DateTime.UtcNow
          .Add(oneDay * -1);
        var dbCacheEntryValid = trackList != null &&
          trackList.UpdatedAt >= dbCacheValidWindow;

        if (dbCacheEntryValid)
        {
          return JsonConvert.DeserializeObject<CachedPlaylist>(
            trackList.Data
          );
        }

        // Both caches missed, get playlist from Spotify
        var playlist = await _api.GetPlaylistAsync(userId, playlistId);
        var tracks = playlist.Tracks.Items
          .Where(t => !t.IsLocal)
          .Select(t => Track.FromSpotify(t.Track))
          .ToList();

        // Get the other pages of tracks
        var page = playlist.Tracks;
        while (page.HasNextPage())
        {
          page = await _api.GetPlaylistTracksAsync(
            userId,
            playlistId,
            offset: page.Offset + page.Limit
          );
          tracks.AddRange(page.Items
            .Where(t => !t.IsLocal)
            .Select(t => Track.FromSpotify(t.Track))
          );
        }

        var cachedPlaylist = new CachedPlaylist
        {
          Playlist = Playlist.FromSpotify(playlist),
          Tracks = tracks
        };

        // Cache the playlist in the default cache
        _background.QueueTask(async token =>
        {
          await _cache.SetAsync(
            $"playlist:{playlistId}",
            cachedPlaylist,
            new DistributedCacheEntryOptions
            {
              SlidingExpiration = oneHour
            }
          );
        });

        // Cache the playlist in the DB
        _background.QueueTask(async token =>
        {
          var playlistJson = JsonConvert.SerializeObject(cachedPlaylist);

          if (trackList == null)
          {
            trackList = new TrackList
            {
              SpotifyPlaylistId = playlistId,
              Data = playlistJson
            };
            _db.Add(trackList);
          }
          else
          {
            trackList.Data = playlistJson;
            trackList.UpdatedAt = DateTime.UtcNow;
          }

          await _db.SaveChangesAsync();
        });

        return cachedPlaylist;
      });
    }
  }
}
