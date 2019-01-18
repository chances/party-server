using Microsoft.AspNetCore.Mvc;
using Server.ViewModels;

namespace Server.Controllers
{
  public class Index : Controller
  {
    [HttpGet]
    [Route("")]
    public IActionResult GetIndex()
    {
      if (HttpContext.User.Identity.IsAuthenticated)
      {
        return View("../Index", new Administrator("Logged in presently."));
      }

      return View("../Index", new Administrator());
    }
  }
}
