namespace Server.Services.Session
{
  public interface ISession
  {
    void Login(string username);
    void Flash(string key, string value);
    void Logout();
  }
}
