namespace Server.ViewModels
{
  public class MobileAuth
  {
    public MobileAuth(string host, string username = null)
    {
      Host = host;
      Username = username;
    }

    public string Username { get; }
    public bool IsAuthenticated => Username != null;

    public string Host { get; }
  }
}
