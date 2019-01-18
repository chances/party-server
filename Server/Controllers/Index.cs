using Microsoft.AspNetCore.Mvc;
using Server.Services;
using Server.ViewModels;

namespace Server.Controllers
{
  public class Index : Controller
  {
    private ProfileProvider _profileProvider;

    public Index(ProfileProvider profileProvider)
    {
      _profileProvider = profileProvider;
    }

    [HttpGet]
    [Route("")]
    public IActionResult GetIndex()
    {
      if (HttpContext.User.Identity.IsAuthenticated)
      {
        return View("../Index", new Administrator(_profileProvider.Profile));
      }

      return View("../Index", new Administrator());
    }
  }
}
