using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Models;
using Server.Data;
using Server.Models;
using Server.Services;
using Server.Services.Authorization;
using Server.Services.Filters;
using Server.Services.Spotify;

namespace Server.Controllers
{
  [Authorize(Roles = Roles.Host)]
  [Route("/playlist")]
  public class Playlists : Controller
  {
    private readonly PartyModelContainer _db;
    private readonly UserProvider _userProvider;
    private readonly SpotifyRepository _spotify;

    public Playlists(PartyModelContainer db, UserProvider user, SpotifyRepository spotify)
    {
      _db = db;
      _userProvider = user;
      _spotify = spotify;
    }

    [HttpGet]
    [Route("")]
    public async Task<IActionResult> Get()
    {
      var currentUser = await _userProvider.GetUserAsync();
      if (currentUser.SpotifyPlaylistId == null)
      {
        return NotFound();
      }

      var playlists = await _spotify.GetMyOwnPlaylistsAsync();
      var playlist = playlists.FirstOrDefault(p => p.Id == currentUser.SpotifyPlaylistId);

      if (playlist == null) return NotFound();

      return Ok(Document.Resource(playlist.Id, playlist));
    }

    [HttpPatch]
    [ValidateModel]
    [Route("")]
    public async Task<IActionResult> Patch(
      [FromBody] ResourceIdentifierDocument<Playlist> patchPlaylist
    )
    {
      var playlists = await _spotify.GetMyOwnPlaylistsAsync();
      var playlist = playlists.FirstOrDefault(p => p.Id == patchPlaylist.Data.Id);

      if (playlist == null) return Error.BadRequest("Invalid playlist id");

      var user = await _userProvider.GetUserAsync();
      user.SpotifyPlaylistId = patchPlaylist.Data.Id;
      user.UpdatedAt = DateTime.UtcNow;

      await _db.SaveChangesAsync();

      // TODO: Update the user's current party somehow? Add the new playlist's tracks or replace the queue?

      return Ok(Document.Resource(playlist.Id, playlist));
    }
  }
}
