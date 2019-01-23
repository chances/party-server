using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Mvc;

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

    [HttpGet]
    [Route("logout")]
    public async Task<IActionResult> Logout()
    {
      await HttpContext.SignOutAsync("Cookies");
      return RedirectToAction(nameof(Home.Index), nameof(Home));
    }
  }
}
