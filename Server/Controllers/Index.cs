using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Models;
using Server.Services;
using Server.ViewModels;

namespace Server.Controllers
{
  public class Index : Controller
  {
    private readonly UserProvider _userProvider;
    private readonly ProfileProvider _profileProvider;
    private readonly SpotifyRepository _spotify;
    private readonly PartyModelContainer _db;

    public Index(
      UserProvider userProvider,
      ProfileProvider profileProvider,
      SpotifyRepository spotify,
      PartyModelContainer db
    )
    {
      _userProvider = userProvider;
      _profileProvider = profileProvider;
      _spotify = spotify;
      _db = db;
    }

    [HttpGet]
    [Route("")]
    public async Task<IActionResult> GetIndex()
    {
      if (_userProvider.IsUserHost)
      {
        var user = _userProvider.User;
        var spotifyProfile = _profileProvider.Profile;
        var playlists = await _spotify.GetMyOwnPlaylists();
        var playlist = playlists
          .FirstOrDefault(p => p.Id == user.SpotifyPlaylistId);
        var party = _db.Party
          .Where(p => p.User.Username == spotifyProfile.Id)
          .FirstOrDefault();

        var admin = new Administrator(spotifyProfile, playlists, playlist, party);

        return View("../Index", admin);
      }

      return View("../Index", new Administrator());
    }
  }
}
