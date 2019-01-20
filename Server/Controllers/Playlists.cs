using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Models;
using Server.Data;
using Server.Models;
using Server.Services;
using Server.Services.Authentication;

namespace Server.Controllers
{
  [Authorize(Roles = Roles.Host)]
  public class Playlists : Controller
  {
    private readonly PartyModelContainer _db;
    private readonly UserProvider _user;
    private readonly SpotifyRepository _spotify;

    public Playlists(PartyModelContainer db, UserProvider user, SpotifyRepository spotify)
    {
      _db = db;
      _user = user;
      _spotify = spotify;
    }

    [HttpGet]
    [Route("/playlist")]
    public async Task<IActionResult> Get()
    {
      var currentUser = _user.User;
      if (currentUser.SpotifyPlaylistId == null)
      {
        return NotFound();
      }

      var playlists = await _spotify.GetMyOwnPlaylists();
      var playlist = playlists.FirstOrDefault(p => p.Id == currentUser.SpotifyPlaylistId);

      if (playlist == null) return NotFound();

      return Ok(Document.Resource(playlist.Id, playlist));
    }

    [HttpPatch]
    [Route("/playlist")]
    public async Task<IActionResult> Patch(
      [FromBody] ResourceIdentifierDocument<Playlist> patchPlaylist
    )
    {
      if (!ModelState.IsValid) return Error.BadRequest(ModelState.Errors());

      var playlists = await _spotify.GetMyOwnPlaylists();
      var playlist = playlists.FirstOrDefault(p => p.Id == patchPlaylist.Data.Id);

      if (playlist != null)
      {
        _user.User.SpotifyPlaylistId = patchPlaylist.Data.Id;
        await _db.SaveChangesAsync();

        // TODO: Update the user's current party somehow? Add the new playlists's tracks or replace the queue?

        return Ok(Document.Resource(playlist.Id, playlist));
      }

      return Error.BadRequest("Invalid playlist id");
    }
  }
}