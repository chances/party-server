using System;
using Nancy;
using Server.Configuration;

namespace Server.Controllers
{
  public sealed class Index : NancyModule
  {
    public Index(AppConfiguration appConfig)
    {
      Get("/", _ => GetIndex());
    }

    private static string GetIndex()
    {
      return @"<a href=""/authentication/redirect/spotify""></a>";
    }
  }
}
