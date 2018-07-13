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

      app.UseOwin(x => x.UseNancy(opt => opt.Bootstrapper = new PartyBootstrapper(appConfig)));
    }
  }
}
