using Microsoft.AspNetCore.Mvc;
using Server.ViewModels;

namespace Server.Controllers
{
  public class Index : Controller
  {
    // GET
    [Route("")]
    public IActionResult GetIndex()
    {
      return View("../Index", new Administrator());
    }
  }
}
