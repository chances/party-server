using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.AspNetCore.Mvc;
using Server.Configuration;
using Server.Data;
using Server.Models;
using Server.Services;
using Server.Services.Authentication;
using Server.ViewModels;

namespace Server.Controllers
{
  [Route("/auth")]
  public class Authentication : Controller
  {
    private readonly AppConfiguration _appConfig;
    private readonly UserProvider _userProvider;

    public Authentication(AppConfiguration appConfig, UserProvider userProvider)
    {
      _appConfig = appConfig;
      _userProvider = userProvider;
    }

    [HttpGet]
    [Route("login")]
    public IActionResult Login(string redirectUri = "/")
    {
      return Challenge(new AuthenticationProperties() { RedirectUri = redirectUri });
    }

    /// <summary>
    /// Responds with a pretty spinner for mobile client users
    /// </summary>
    [HttpGet]
    [Route("mobile")]
    public ViewResult Mobile()
    {
      var host = new Uri(HttpContext.Request.GetEncodedUrl()).Host;
      return View("../MobileAuth", new MobileAuth(host));
    }

    /// <summary>
    /// Displays a success message and is suitable for detection in mobile app
    /// </summary>
    [HttpGet]
    [Authorize(Roles = Roles.Authenticated)]
    [Route("finished")]
    public ViewResult Finished()
    {
      var host = new Uri(HttpContext.Request.GetEncodedUrl()).Host;
      var username = HttpContext.User?.Username();

      return View("../MobileAuth", new MobileAuth(host, username));
    }

    [HttpGet]
    [Route("token")]
    public ActionResult<Document<SpotifyToken>> Token()
    {
      if (!_userProvider.IsUserHost) return Unauthorized();

      var token = SpotifyAuthenticationScheme.GetToken(User.Claims.ToList());
      if (token == null) return Unauthorized();

      var tokenResponse = new SpotifyToken(
        token.AccessToken,
        token.CreateDate.AddSeconds(token.ExpiresIn)
      );

      return Document.Resource(User.Username(), tokenResponse);
    }

    [HttpGet]
    [Route("logout")]
    public async Task<IActionResult> Logout()
    {
      await HttpContext.SignOutAsync("Cookies");
      return RedirectToAction(nameof(Home.Index), nameof(Home));
    }
  }
}
