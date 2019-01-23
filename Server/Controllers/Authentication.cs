using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Server.Services;
using Server.Services.Authentication;
using Server.ViewModels;

namespace Server.Controllers
{
  [Route("/auth")]
  public class Authentication : Controller
  {
    [HttpGet]
    [Route("login")]
    public IActionResult Login(string returnUrl = "/")
    {
      return Challenge(new AuthenticationProperties() { RedirectUri = returnUrl });
    }

    /// <summary>
    /// Responds with a pretty spinner for mobile client users
    /// </summary>
    [HttpGet]
    [Route("mobile")]
    public ViewResult Mobile()
    {
      return View("../MobileAuth", new MobileAuth());
    }

    /// <summary>
    /// Displays a success message and is suitable for detection in mobile app
    /// </summary>
    [HttpGet]
    [Authorize(Roles = Roles.Authenticated)]
    [Route("finished")]
    public ViewResult Finished()
    {
      var username = HttpContext.User?.Username();
      return View("../MobileAuth", new MobileAuth(username));
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
