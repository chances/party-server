namespace Server.ViewModels
{
  public class MobileAuth
  {
    public MobileAuth(string username = null)
    {
      Username = username;
    }

    public string Username { get; }
    public bool IsAuthenticated => Username != null;
  }
}
