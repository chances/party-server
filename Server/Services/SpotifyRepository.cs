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

namespace Server.Services
{
  public class SpotifyRepository : ScopedService
  {
    private static readonly IEnumerable<SimplePlaylist> EmptyList = new List<SimplePlaylist>(0);

    private readonly IDistributedCache _cache;
    private readonly Token _spotifyAccessToken;

    public SpotifyRepository(
      IHttpContextAccessor context,
      IDistributedCache cache
    ) : base(context)
    {
      if (HttpContext.User.IsInRole(Roles.Host))
      {
        _spotifyAccessToken = SpotifyAuthenticationScheme.GetToken(HttpContext.User.Claims);
      }
    }

    public async Task<IEnumerable<SimplePlaylist>> GetMyOwnPlaylists()
    {
      var user = HttpContext.User;
      if (!user.IsInRole(Roles.Host))
      {
        return EmptyList;
      }

      var userId = user.Claims.First(c => c.Type == ClaimTypes.NameIdentifier).Value;

      var playlists = await GetMyPlaylists();
      return playlists.Where(p => p.Owner.Id == userId);
    }

    public async Task<IEnumerable<SimplePlaylist>> GetMyPlaylists()
    {
      var user = HttpContext.User;
      if (!user.IsInRole(Roles.Host))
      {
        return EmptyList;
      }

      var userId = user.Claims.First(c => c.Type == ClaimTypes.NameIdentifier).Value;

      if (_spotifyAccessToken == null) return EmptyList;

      // TODO: Try to get the playlists from the cache

      var api = new SpotifyWebAPI()
      {
        UseAuth = true,
        AccessToken = _spotifyAccessToken.AccessToken,
        TokenType = _spotifyAccessToken.TokenType
      };

      var playlists = await api.GetUserPlaylistsAsync(userId, 50);

      // TODO: Cache the retrieved playlists

      return playlists.Items;
    }
  }
}
