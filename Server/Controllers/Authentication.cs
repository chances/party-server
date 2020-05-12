using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using Server.Configuration;
using Server.Data;
using Server.Models;
using Server.Services;
using Server.Services.Authentication;
using Server.Services.Authorization;
using Server.ViewModels;

namespace Server.Controllers
{
  [Route("/auth")]
  public class Authentication : Controller
  {
    private readonly AppConfiguration _appConfig;
    private readonly UserProvider _userProvider;
    private readonly ILogger _logger;

    public Authentication(AppConfiguration appConfig, UserProvider userProvider, ILogger<Authentication> logger)
    {
      _appConfig = appConfig;
      _userProvider = userProvider;
      _logger = logger;
    }

    [HttpGet]
    [Route("login")]
    public IActionResult Login(string redirectUri = "/", params string[] schemes)
    {
      // Use a specific Auth0 social connection based on given schemes
      var authProperties = new Dictionary<string, string>();
      if (schemes?.Contains(SpotifyAuthenticationScheme.Name) ?? false)
      {
        authProperties.Add("connection", Auth0AuthenticationScheme.SpotifyConnection);
      }

      return Challenge(new AuthenticationProperties(authProperties)
      {
        RedirectUri = redirectUri
      }, schemes);
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
      var username = HttpContext.User.Identity.Name;

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

      return Document.Resource(User.Identity.Name, tokenResponse);
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
