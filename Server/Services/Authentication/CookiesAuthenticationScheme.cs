using System;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Models;

namespace Server.Services.Authentication
{
  public static class CookiesAuthenticationScheme
  {
    public static readonly string Name = "Cookies";

    private static readonly string CookieName = "cpSESSION";
    private static readonly string ProductionCookieDomain = ".chancesnow.me";

    public static void Configure(CookieAuthenticationOptions options, ITicketStore sessionStore, bool isProduction)
    {
      options.SessionStore = sessionStore;
      options.LoginPath = "/auth/login";
      options.LogoutPath = "/auth/logout";
      options.ReturnUrlParameter = "return_to";
      options.ExpireTimeSpan = TimeSpan.FromHours(12);
      options.Cookie.Name = CookieName;
      options.Cookie.SecurePolicy = isProduction ? CookieSecurePolicy.Always : CookieSecurePolicy.None;
      options.Cookie.Domain = isProduction ? ProductionCookieDomain : "";

      options.Events = new CookieAuthenticationEvents
      {
        OnSignedIn = async context =>
        {
          var dbContext = context.HttpContext.RequestServices.GetRequiredService<PartyModelContainer>();
          await SpotifyAuthenticationScheme.UpsertPartyUser(dbContext, context.Principal.Claims);
        },

        OnValidatePrincipal = async context =>
        {
          var principalExpired = await SpotifyAuthenticationScheme.RefreshAccessToken(context.Principal.Claims);

          // Reject principal if Spotify access token is expired
          if (principalExpired)
          {
            context.RejectPrincipal();
          }
        }
      };
    }
  }
}
