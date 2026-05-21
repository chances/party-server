using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Db = Models;
using Server.Data;
using Server.Models;
using Server.Services;
using Newtonsoft.Json;
using Server.Services.Authentication;
using Server.Services.Authorization;
using Server.Services.Channels;
using Server.Services.Filters;
using Server.Services.Spotify;

namespace Server.Controllers
{
  [Route("/party")]
  public class Party : Controller
  {
    private readonly RoomCodeGenerator _roomCodeGenerator;
    private readonly UserProvider _userProvider;
    private readonly PartyProvider _partyProvider;
    private readonly SpotifyRepository _spotify;
    private readonly IEventChannel<PublicParty> _partyChannel;
    private readonly Db.PartyModelContainer _db;

    public Party(
      RoomCodeGenerator roomCodeGenerator,
      UserProvider userProvider,
      PartyProvider partyProvider,
      SpotifyRepository spotify,
      IEventChannel<PublicParty> partyChannel,
      Db.PartyModelContainer db
    )
    {
      _roomCodeGenerator = roomCodeGenerator;
      _userProvider = userProvider;
      _partyProvider = partyProvider;
      _spotify = spotify;
      _partyChannel = partyChannel;
      _db = db;
    }

    [HttpGet]
    [AuthorizeForApiAudiences]
    [Route("")]
    public async Task<IActionResult> Index()
    {
      var party = await _partyProvider.GetCurrentPartyAsync();
      if (party == null) return NotFound();

      var guests = await party.GuestList(_db);

      return AugmentParty(party, guests);
    }

    [HttpPost]
    [AuthorizeForApiAudiences]
    [Authorize(Roles = Roles.Host)]
    [ValidateModel]
    [Route("start")]
    public async Task<IActionResult> Start([FromBody] NewResourceDocument<NewParty> newParty)
    {
      var playlistId = newParty.Data.Attributes.PlaylistId;

      var user = await _userProvider.GetUserAsync(bypassCache: true);
      if (user == null) return Unauthorized();

      // Bad request if the user hasn't ended an ongoing party
      var currentParty = user.Party;
      if (currentParty != null)
      {
        return Error.BadRequest("A party has already been started. End it first.");
        // TODO: Call end party action?
      }

      var playlists = await _spotify.GetMyPlaylistsAsync();
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

    [HttpPost]
    [AuthorizeForApiAudiences]
    [Authorize(Roles = Roles.Host)]
    [Route("end")]
    public async Task<IActionResult> End()
    {
      var user = await _userProvider.GetUserAsync(bypassCache: true);
      if (user == null) return Unauthorized();

      var currentParty = user.Party;
      if (currentParty == null)
      {
        return Error.BadRequest("No party exists for current user");
      }

      currentParty.Ended = true;
      user.Party = null;

      await _db.SaveChangesAsync();

      var publicParty = PublicParty.FromParty(currentParty);

      _partyChannel.Push(publicParty);

      return Ok(Document.Resource(currentParty.RoomCode, publicParty));
    }

    [HttpPost]
    [ValidateModel]
    [Route("join")]
    public async Task<IActionResult> Join([FromBody] NewResourceDocument<JoinParty> joinParty)
    {
      var roomCode = joinParty.Data.Attributes.RoomCode;
      var party = await _db.Party.FirstOrDefaultAsync(p => p.RoomCode == roomCode);

      if (party == null) return NotFound();

      // If the party has ended, respond with just the party
      if (party.Ended) return Ok(Document.Resource(
        party.RoomCode, PublicParty.FromParty(party)
      ));

      var guests = await party.GuestList(_db);

      // If the user is already authenticated, bail and respond with their existing party
      if (HttpContext.User?.Identity.IsAuthenticated ?? false)
      {
        return AugmentParty(party, guests);
      }

      Guest guest = null;

      // TODO: Add party leave endpoint for guests? (Remove them from Redis, especially)

      // Or remove guest tokens from Redis when a party ends? (Periodic cleanup of guests)
      // "Already joined a party" will error if guest exists in Redis, but not DB

      // It is a bad request to try to join a party if the guest has already
      //  joined a different party
      //
      // If the user is already a guest and they've joined _this_ party then
      //  respond with augmented party
      // Otherwise, it is a bad request if they have NOT joined _this_ party
      if (_userProvider.IsUserGuest)
      {
        guest = _userProvider.Guest;
        if (guest.PartyId == party.Id)
        {
          return AugmentParty(party, guests);
        }

        return Error.BadRequest("Already joined a party");
      }

      if (!HttpContext.Request.Headers.ContainsKey("Origin")) return BadRequest();
      var origin = HttpContext.Request.Headers["Origin"];

      guest = new Guest
      {
        Origin = origin,
        PartyId = party.Id
      };

      guests.Add(guest);
      party.UpdateGuestList(guests);
      await _db.SaveChangesAsync();

      var publicParty = PublicParty.FromParty(party, guests);

      _partyChannel.Push(publicParty);

      var principal = CookiesAuthenticationScheme.CreateGuestPrincipal(guest);
      await HttpContext.SignInAsync(CookiesAuthenticationScheme.Name, principal);

      return new OkObjectResult(Document.Resource(party.RoomCode, publicParty));
    }

    private static OkObjectResult AugmentParty(Db.Party party, IEnumerable<Guest> guests)
    {
      var publicParty = PublicParty.FromParty(party, guests);
      return new OkObjectResult(Document.Resource(party.RoomCode, publicParty));
    }

    [HttpGet]
    [AuthorizeForApiAudiences]
    [Route("ping")]
    public OkObjectResult Ping()
    {
      // QUESTION: Should guest pinging return anything else?
      // NOTE: This tiny amount of work keeps the request blazing fast...

      return Ok(Document.Resource("pong", Pong.Instance));
    }

    [HttpGet]
    [AuthorizeForApiAudiences]
    [Route("queue")]
    public async Task<IActionResult> GetQueue()
    {
      var currentParty = await _partyProvider.GetCurrentPartyAsync();
      if (currentParty == null) return NotFound();

      // TODO: Add pagination
      // TODO: Limit the list to ~5-10 tracks for party guests

      var queue = await currentParty.QueueTracks(_db);

      return Ok(Document.Collection(queue.Tracks, t => t.Id));
    }

    [HttpGet]
    [AuthorizeForApiAudiences]
    [Route("history")]
    public async Task<IActionResult> GetHistory()
    {
      var currentParty = await _partyProvider.GetCurrentPartyAsync();
      if (currentParty == null) return NotFound();

      // TODO: Add pagination

      var history = await currentParty.HistoryTracks(_db);

      return Ok(Document.Collection(history.Tracks, t => t.Id));
    }
  }
}
