using System;
using System.IO;
using dotenv.net;
using Microsoft.Extensions.Configuration.EnvironmentVariables;

namespace Server.Configuration
{
  public class DotEnvConfigurationProvider : EnvironmentVariablesConfigurationProvider
  {
    public override void Load()
    {
      DotEnv.Config(false);
      if (!File.Exists(".env"))
      {
        Console.WriteLine("Warning: .env file is not present. Using system provided environment variables");
      }
      base.Load();
    }
  }
}
