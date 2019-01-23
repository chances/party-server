using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.Redis;
using Microsoft.Extensions.DependencyInjection;
using Models;
using Server.Configuration;
using Server.Services;
using Server.Services.Authentication;
using Server.Services.Background;
using Server.Services.Spotify;

namespace Server
{
  public class Startup
  {
    private static readonly AppConfiguration _appConfig = new AppConfiguration();
    private readonly RedisCache _redisCache = new RedisCache(new RedisCacheOptions()
    {
      Configuration = _appConfig.RedisConnectionString
    });

    // This method gets called by the runtime. Use this method to add services to the container.
    // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
    public void ConfigureServices(IServiceCollection services)
    {
      services.AddSingleton(_appConfig);
      services.AddSingleton(_redisCache);
      services.AddDbContextPool<PartyModelContainer>(options => options.UseNpgsql(_appConfig.ConnectionString), 32);

      // Background tasks
      services.AddHostedService<QueuedHostedService>();
      services.AddSingleton<IBackgroundTaskQueue, BackgroundTaskQueue>();

      // Authentication
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
          new RedisCacheTicketStore(_redisCache),
          _appConfig
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

      // Controller services
      services.AddHttpContextAccessor();
      services.AddScoped<UserProvider>();
      services.AddScoped<ProfileProvider>();
      services.AddScoped<SpotifyRepository>();

      services.AddSingleton(new RoomCodeGenerator());

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
