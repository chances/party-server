using System;
using System.Linq;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Cors.Infrastructure;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Infrastructure;
using Microsoft.AspNetCore.Mvc.Routing;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Caching.StackExchangeRedis;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Net.Http.Headers;
using Models;
using Newtonsoft.Json;
using Server.Configuration;
using Server.Data;
using Server.Hubs;
using Server.Middleware;
using Server.Models;
using Server.Services;
using Server.Services.Authentication;
using Server.Services.Background;
using Server.Services.Channels;
using Server.Services.Jobs;
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
      services.AddDbContextPool<PartyModelContainer>(options => options.UseNpgsql(_appConfig.ConnectionString), 15);

      services.AddLogging(
        builder =>
        {
          builder.SetMinimumLevel(_appConfig.Mode == Mode.Development ? LogLevel.Debug : LogLevel.Warning)
            .AddFilter("Microsoft", LogLevel.Warning)
            .AddFilter("System", LogLevel.Warning)
            .AddConsole();
        });

      // Background tasks
      services.AddHostedService<QueuedHostedService>();
      services.AddSingleton<IBackgroundTaskQueue, BackgroundTaskQueue>();
      services.AddHostedService<PruneExpiredGuestsService>();

      // CORS
      services.AddCors(options => options.AddDefaultPolicy(ConfigureCorsPolicy));

      // Authentication
      services.AddStackExchangeRedisCache(options =>
      {
        options.Configuration = _appConfig.RedisConnectionString;
        options.InstanceName = "redis";
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
          _appConfig.Mode,
          _appConfig.Spotify
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
      services.AddSingleton<IActionContextAccessor, ActionContextAccessor>();
      services.AddScoped(sp => {
        var actionContext = sp.GetRequiredService<IActionContextAccessor>().ActionContext;
        var factory = sp.GetRequiredService<IUrlHelperFactory>();
        return factory.GetUrlHelper(actionContext);
      });
      services.AddScoped<ProfileProvider>();
      services.AddScoped<UserProvider>();
      services.AddScoped<PartyProvider>();
      services.AddScoped<SpotifyRepository>();

      services.AddSingleton(new RoomCodeGenerator());

      // SignalR real-time hubs and channels
      services.AddSignalR().AddJsonProtocol(options =>
      {
        options.PayloadSerializerOptions.WriteIndented = false;
      });

      services.AddSingleton<IEventChannel<PublicParty>>(new EventChannel<PublicParty>());
      services.AddSingleton<IEventChannel<Resource<Queue>>>(new EventChannel<Resource<Queue>>());
      services.AddSingleton<IEventChannel<Resource<History>>>(new EventChannel<Resource<History>>());

      services
        .AddMvc(options => {
          options.EnableEndpointRouting = false;
        })
        .AddNewtonsoftJson(options =>
        {
          options.SerializerSettings.DateFormatHandling = DateFormatHandling.IsoDateFormat;
          options.SerializerSettings.DateTimeZoneHandling = DateTimeZoneHandling.Utc;
          options.SerializerSettings.ReferenceLoopHandling = ReferenceLoopHandling.Ignore;
        });
    }

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
      if (_appConfig.Mode == Mode.Development)
      {
        app.UseDeveloperExceptionPage();
      }

      app.UseStaticFiles();
      app.UseCors();
      app.UseAuthentication();

      if (_appConfig.Mode != Mode.Development) {
        app.UseHsts();
      }

      app.UseWebSocketOriginPolicy(_appConfig.Cors.AllowedOrigins);
      app.Map("/events", map =>
      {
        map.UseSignalR(route =>
        {
          route.MapHub<PartyHub>("/party");
        });
      });

      app.UseMvc();
    }

    private static void ConfigureCorsPolicy(CorsPolicyBuilder builder)
    {
      var allowedHeaders = new[]
      {
        HeaderNames.CacheControl, HeaderNames.ContentLanguage, HeaderNames.Accept,
        HeaderNames.Expires, HeaderNames.LastModified, "X-Requested-With",
        HeaderNames.ContentLength, HeaderNames.ContentType, "Last-Event-ID"
      };
      var allowedOrigins = _appConfig.Cors.AllowedOrigins
        .Select(allowedOrigin => new Uri(allowedOrigin))
        .ToArray();
      builder.SetIsOriginAllowed(origin =>
      {
        var originUri = new Uri(origin);
        return allowedOrigins.Any(allowedOrigin =>
        {
          var allowed = string.Equals(allowedOrigin.Scheme, originUri.Scheme) &&
                        string.Equals(allowedOrigin.Host, originUri.Host) &&
                        allowedOrigin.Port == originUri.Port;
          return allowed;
        });
      });
      builder.WithMethods("GET", "PUT", "POST", "PATCH", "DELETE");
      builder.WithHeaders(allowedHeaders);
      builder.WithExposedHeaders(allowedHeaders);
      builder.AllowCredentials();
      builder.SetPreflightMaxAge(TimeSpan.FromHours(12));
    }
  }
}
