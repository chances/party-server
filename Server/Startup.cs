using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Nancy.Owin;
using Server.Configuration;

namespace Server
{
  public class Startup
  {
    private readonly IConfiguration config;
    
    public Startup(IHostingEnvironment environment)
    {
      var builder = new ConfigurationBuilder()
        .AddDotEnv(optionsBuilder => optionsBuilder.AddThrowOnError(false))
        .SetBasePath(environment.ContentRootPath);

      config = builder.Build();
    }
    
    public void Configure(IApplicationBuilder app)
    {
      var appConfig = new AppConfiguration();
      config.Bind(appConfig);

      app.UseOwin(x => x.UseNancy(opt => opt.Bootstrapper = new PartyBootstrapper(appConfig)));
    }
  }
}
