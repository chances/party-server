using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Models;
using Newtonsoft.Json;
using Server.Models.Spotify;
using Server.Services;
using Server.Services.Authorization;
using Server.Services.Spotify;
using Server.ViewModels;

namespace Server.Controllers
{
  public class Home : Controller
  {
    private readonly PartyProvider _partyProvider;
    private readonly SpotifyRepository _spotify;
    private readonly PartyModelContainer _db;

    public Home(
      PartyProvider userProvider,
      SpotifyRepository spotify,
      PartyModelContainer db
    )
    {
      _partyProvider = userProvider;
      _spotify = spotify;
      _db = db;
    }

    [HttpGet]
    [Route("")]
    public async Task<IActionResult> Index()
    {
      if (!HttpContext.User.IsInRole(Roles.Host)) return View("../Index", new Administrator());

      var playlists = (await _spotify.GetMyOwnPlaylistsAsync()).ToList();
      var party = await _partyProvider.GetCurrentPartyAsync();
      var playlist = party != null
        ? playlists.FirstOrDefault(p => p.Id == party.User.SpotifyPlaylistId)
        : null;
      string spotifyProfileJson = null;
      if (party == null)
      {
        var user = await UserProvider.GetUserAsync(HttpContext.User.Identity.Name, _db);
        spotifyProfileJson = user.SpotifyUser;
      }
      else
      {
        spotifyProfileJson = party.User.SpotifyUser;
      }
      var spotifyProfile = JsonConvert.DeserializeObject<PrivateProfile>(spotifyProfileJson);

      var admin = new Administrator(spotifyProfile, playlists, party, playlist);
      return View("../Index", admin);
    }
  }
}
