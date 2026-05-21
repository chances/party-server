namespace Server.Services.Authorization
{
  public static class Roles
  {
    public static string[] All = new string[] { Host, Guest };
    public const string Host = "host";
    public const string Guest = "guest";
    public const string Authenticated = "host,guest";
  }
}
