using System.Text;
using JetBrains.Annotations;
using Nancy;

namespace Server.Controllers
{
  [UsedImplicitly]
  public sealed class Index : NancyModule
  {
    public Index()
    {
      Get("/", _ => GetIndex());
    }

    private Response GetIndex()
    {
      return Response.AsText(@"<a href=""/auth/spotify"">Login with Spotify</a>", "text/html", Encoding.UTF8);
    }
  }
}
