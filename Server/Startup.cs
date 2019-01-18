using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Models;
using Server.Configuration;
using Server.Services.Authentication;

namespace Server
{
  public class Startup
  {
    private readonly AppConfiguration _appConfig = new AppConfiguration();

    // This method gets called by the runtime. Use this method to add services to the container.
    // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
    public void ConfigureServices(IServiceCollection services)
    {
      services.AddSingleton(typeof(AppConfiguration), _appConfig);
      services.AddDbContextPool<PartyModelContainer>(options => options.UseNpgsql(_appConfig.ConnectionString), 32);
      services.AddDistributedRedisCache(options =>
      {
        options.Configuration = _appConfig.RedisConnectionString;
      });
      services.AddAuthentication(options =>
      {
        options.DefaultAuthenticateScheme = CookiesAuthenticationScheme.Name;
        options.DefaultSignInScheme = CookiesAuthenticationScheme.Name;
        options.DefaultChallengeScheme = SpotifyAuthenticationScheme.Name;
      })
      .AddCookie(
        CookiesAuthenticationScheme.Name,
        (options) => CookiesAuthenticationScheme.Configure(
          options,
          new RedisCacheTicketStore(_appConfig.RedisConnectionString),
          _appConfig.Mode.IsProduction()
        )
      )
      .AddOAuth(
        SpotifyAuthenticationScheme.Name,
        (options) => SpotifyAuthenticationScheme.Configure(
          options,
          _appConfig.Spotify.AppKey,
          _appConfig.Spotify.AppSecret,
          _appConfig.Spotify.Callback)
      );
      services.AddMvc();
    }

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IHostingEnvironment env)
    {
      if (_appConfig.Mode == Mode.Development)
      {
        app.UseDeveloperExceptionPage();
      }

      app.UseStaticFiles();
      app.UseAuthentication();
      app.UseMvc();
    }
  }
}
