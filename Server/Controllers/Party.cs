using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Db = Models;
using Server.Data;
using Server.Models;
using Server.Services;
using Newtonsoft.Json;
using Server.Services.Spotify;

namespace Server.Controllers
{
  [Route("/party")]
  public class Party : Controller
  {
    private readonly RoomCodeGenerator _roomCodeGenerator;
    private readonly UserProvider _userProvider;
    private readonly SpotifyRepository _spotify;
    private readonly Db.PartyModelContainer _db;

    public Party(
      RoomCodeGenerator roomCodeGenerator,
      UserProvider userProvider,
      SpotifyRepository spotify,
      Db.PartyModelContainer db
    )
    {
      _roomCodeGenerator = roomCodeGenerator;
      _userProvider = userProvider;
      _spotify = spotify;
      _db = db;
    }

    [HttpGet]
    [Route("")]
    public async Task<IActionResult> Index()
    {
      var user = await _userProvider.GetUserAsync();
      if (user == null) return Unauthorized();

      var party = user.Party;
      if (party == null) return NotFound();

      return Ok(Document.Resource(
        party.Id.ToString(),
        PublicParty.FromParty(party)
      ));
    }

    [HttpPost]
    [Route("start")]
    public async Task<IActionResult> Start([FromBody] NewResourceDocument<NewParty> newParty)
    {
      if (!ModelState.IsValid) return Error.BadRequest(ModelState.Errors());

      var playlistId = newParty.Data.Attributes.PlaylistId;

      var user = await _userProvider.GetUserAsync(_db);
      if (user == null) return Unauthorized();

      // Bad request if the user hasn't ended an ongoing party
      var currentParty = user.Party;
      if (currentParty != null)
      {
        return Error.BadRequest("A party has already been started. End it first.");
        // TODO: Call end party action?
      }

      var playlists = await _spotify.GetMyPlaylists();
      var currentPlaylist = playlists.FirstOrDefault(p => p.Id == playlistId);

      // If the Playlist doesn't belong to user, bad request
      if (currentPlaylist == null)
      {
        return Error.BadRequest(
          $"Invalid playlist {playlistId}. User '{user.Username}' does not own or subscribe to given playlist"
        );
      }

      var roomCode = _roomCodeGenerator.NewRoomCode;
      if (string.IsNullOrWhiteSpace(roomCode))
      {
        return Error.Internal("Could not generate room code");
      }

      var location = newParty.Data.Attributes.Location;

      var party = new Db.Party
      {
        RoomCode = roomCode,
        Location = JsonConvert.SerializeObject(location)
      };
      user.Party = party;
      user.UpdatedAt = DateTime.UtcNow;

      var playlist = await _spotify
        .GetPlaylistAsync(user.Username, currentPlaylist.Id);

      // New party's queue
      party.Queue = new Db.TrackList
      {
        Data = JsonConvert.SerializeObject(playlist.Tracks)
      };
      _db.TrackList.Add(party.Queue);

      // New party's playback history
      party.History = new Db.TrackList
      {
        Data = JsonConvert.SerializeObject(new List<object>(0))
      };
      _db.TrackList.Add(party.History);

      // New party's guest list
      party.Guests = new Db.GuestList
      {
        Data = JsonConvert.SerializeObject(new List<object>(0))
      };

      await _db.SaveChangesAsync();

      return Ok(Document.ResourceIdentifier<Db.Party>(roomCode));
    }
  }
}
