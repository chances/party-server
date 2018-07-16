using Newtonsoft.Json.Serialization;

namespace Server.Services
{
  public class Json
  {
    public static DefaultContractResolver SnakeCaseContractResolver = new DefaultContractResolver
    {
      NamingStrategy = new SnakeCaseNamingStrategy()
    };
  }
}
