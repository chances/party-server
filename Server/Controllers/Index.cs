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

    private static string GetIndex()
    {
      return @"<a href=""/auth/spotify"">Login with Spotify</a>";
    }
  }
}
