namespace Server.Services.Cache
{
  public interface ISerializable
  {
    string Serialize();
    object Deserialize(string value);
  }
}
