using System;
using dotenv.net;
using dotenv.net.DependencyInjection.Infrastructure;
using Microsoft.Extensions.Configuration;

namespace Server.Configuration
{
  public static class DotEnvConfigurationExtensions
  {
    public static IConfigurationBuilder AddDotEnv(this IConfigurationBuilder builder,
      Action<DotEnvOptionsBuilder> setupAction)
    {
      if (builder == null)
      {
        throw new ArgumentNullException(nameof(builder));
      }
      if (setupAction == null)
      {
        throw new ArgumentNullException(nameof(setupAction));
      }
      
      var dotEnvOptionsBuilder = new DotEnvOptionsBuilder();
      setupAction(dotEnvOptionsBuilder);
            
      var dotEnvOptions = dotEnvOptionsBuilder.Build();
      DotEnv.Config(dotEnvOptions);
            
      return builder;
    }
  }
}
