using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Nancy.Owin;
using Server.Configuration;

namespace Server
{
  public class Startup
  {
    private readonly IConfiguration _config;

    public Startup(IHostingEnvironment environment)
    {
      var builder = new ConfigurationBuilder()
        .SetBasePath(environment.ContentRootPath);

      _config = builder.Build();
    }

    public void Configure(IApplicationBuilder app)
    {
      var appConfig = new AppConfiguration();

      Console.WriteLine(appConfig.Port);
      Console.WriteLine(appConfig.Cors.AllowedOrigins);
      Console.WriteLine(appConfig.Spotify.AppKey);
      if (Environment.GetEnvironmentVariables() is Dictionary<string, string> envVars)
        Console.WriteLine(envVars.Keys.Distinct().Aggregate((a, b) => a + "\n" + b));
      Console.WriteLine(appConfig.Spotify.AppSecret);
      Console.WriteLine(appConfig.Spotify.Callback);

      app.UseOwin(x => x.UseNancy(opt => opt.Bootstrapper = new PartyBootstrapper(appConfig)));
    }
  }
}
