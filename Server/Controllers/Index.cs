using Microsoft.AspNetCore.Mvc;

namespace Server.Controllers
{
  public class Index : Controller
  {
    // GET
    public IActionResult GetIndex()
    {
      return View("../Index");
    }
  }
}
