using System;
namespace Server.Services.Authentication
{
  public static class Roles
  {
    public const string Host = "host";
    public const string Guest = "guest";
    public const string Authenticated = "host,guest";
  }
}
