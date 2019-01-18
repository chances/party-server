using System;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using Microsoft.AspNetCore.Authentication.OAuth;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Models;
using Spotify.API.NetCore.Models;
using Server.Configuration;
using Server.Services.Authentication;
using Newtonsoft.Json;
using System.Security.Claims;
using System.Collections.Generic;
using Microsoft.AspNetCore.Authentication.Cookies;

namespace Server
{
  public class Startup
  {
    private static readonly string SpotifyAccessTokenClaim = "urn:party:spotify:accessToken";
    private static readonly string SpotifyRefreshTokenClaim = "urn:party:spotify:refreshToken";
    private static readonly string SpotifyTokenExpiryClaim = "urn:party:spotify:tokenExpiry";
    private static readonly string SpotifyUserJsonClaim = "urn:party:spotify:userJson";

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
      services.AddAuthentication(options => {
        options.DefaultAuthenticateScheme = "Cookies";
        options.DefaultSignInScheme = "Cookies";
        options.DefaultChallengeScheme = "Spotify";
      })
        .AddCookie("Cookies", "cpSESSION", (options) => {
          options.SessionStore = new RedisCacheTicketStore(_appConfig.RedisConnectionString);
          options.LoginPath = "/auth/login";
          options.LogoutPath = "/auth/logout";
          options.ReturnUrlParameter = "return_to";
          options.ExpireTimeSpan = TimeSpan.FromHours(12);
          options.Cookie.Name = "cpSESSION";
          options.Cookie.SecurePolicy = _appConfig.Mode.IsProduction() ? CookieSecurePolicy.Always : CookieSecurePolicy.None;
          options.Cookie.Domain = _appConfig.Mode.IsProduction() ? ".chancesnow.me" : "";

          options.Events = new CookieAuthenticationEvents
          {
            OnSignedIn = async context =>
            {
              var claims = context.Principal.Claims
                .ToDictionary(claim => claim.Type);
              var userJson = claims[SpotifyUserJsonClaim].Value;
              var accessToken = claims[SpotifyAccessTokenClaim].Value;
              var refreshToken = claims[SpotifyRefreshTokenClaim].Value;
              var tokenExpiry = claims[SpotifyTokenExpiryClaim].Value;

              var spotifyUser = JsonConvert.DeserializeObject<PrivateProfile>(userJson);

              var dbContext = context.HttpContext.RequestServices.GetRequiredService<PartyModelContainer>();
              var user = await dbContext.User
                .Where(u => u.Username == spotifyUser.Id)
                .DefaultIfEmpty(new User()
                {
                  Username = spotifyUser.Id,
                })
                .FirstOrDefaultAsync();

              if (dbContext.Entry(user).State == EntityState.Detached)
              {
                dbContext.User.Add(user);
              }
              else
              {
                user.UpdatedAt = DateTime.UtcNow;
              }

              user.AccessToken = accessToken;
              user.RefreshToken = refreshToken;
              user.SpotifyUser = userJson;
              user.TokenExpiryDate = DateTime.Parse(tokenExpiry).ToUniversalTime();

              await dbContext.SaveChangesAsync();
            },

            OnValidatePrincipal = async context =>
            {
              var claims = context.Principal.Claims
                .ToDictionary(claim => claim.Type);
              var userJson = claims[SpotifyUserJsonClaim].Value;
              var accessToken = claims[SpotifyAccessTokenClaim].Value;
              var refreshToken = claims[SpotifyRefreshTokenClaim].Value;
              var tokenExpiry = DateTime.Parse(claims[SpotifyTokenExpiryClaim].Value).ToUniversalTime();

              // Reject principal if Spotify access token is expired
              if (tokenExpiry.Subtract(DateTime.UtcNow).Ticks <= 0)
              {
                // TODO: Try to refresh Spotify access token
                context.RejectPrincipal();
              }
            }
          };
        })
        .AddOAuth("Spotify", (options) => {
          options.ClientId = _appConfig.Spotify.AppKey;
          options.ClientSecret = _appConfig.Spotify.AppSecret;
          options.CallbackPath = _appConfig.Spotify.Callback;
          options.AuthorizationEndpoint = "https://accounts.spotify.com/authorize";
          options.TokenEndpoint = "https://accounts.spotify.com/api/token";
          options.UserInformationEndpoint = "https://api.spotify.com/v1/me";
          options.SaveTokens = true;

          options.Events = new OAuthEvents
          {
            OnCreatingTicket = async context =>
            {
              var tokenExpiry = DateTime.UtcNow.AddSeconds(double.Parse(context.TokenResponse.ExpiresIn));

              var request = new HttpRequestMessage(HttpMethod.Get, context.Options.UserInformationEndpoint);
              request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
              request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", context.AccessToken);

              var response = await context.Backchannel.SendAsync(request, HttpCompletionOption.ResponseHeadersRead, context.HttpContext.RequestAborted);
              response.EnsureSuccessStatusCode();

              var userJson = await response.Content.ReadAsStringAsync();
              var spotifyUser = JsonConvert.DeserializeObject<PrivateProfile>(userJson);

              var claims = new List<Claim>
              {
                new Claim(ClaimTypes.NameIdentifier, spotifyUser.Id),
                new Claim(SpotifyAccessTokenClaim, context.AccessToken),
                new Claim(SpotifyRefreshTokenClaim, context.RefreshToken),
                new Claim(SpotifyTokenExpiryClaim, tokenExpiry.ToString()),
                new Claim(SpotifyUserJsonClaim, userJson)
              };
              context.Principal.AddIdentity(new ClaimsIdentity(claims, "Spotify"));
            }
          };
        });
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
