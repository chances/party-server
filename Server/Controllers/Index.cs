using System;
using Nancy;

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
      return "Hello from Party";
    }
  }
}
