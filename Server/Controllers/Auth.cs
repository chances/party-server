using Nancy;

namespace Server.Controllers
{
  public class Auth : NancyModule
  {
    public Auth()

    {
      Get("/finished", _ => GetFinished());
    }

    private dynamic GetFinished()
    {
      return $"Logged in as {Context.CurrentUser.Identity.Name}";
    }
  }
}
