using Microsoft.Extensions.Configuration;

namespace Server.Configuration
{
  public class DotEnvConfigurationSource : IConfigurationSource
  {
    public IConfigurationProvider Build(IConfigurationBuilder builder)
    {
      return new DotEnvConfigurationProvider();
    }
  }
}
