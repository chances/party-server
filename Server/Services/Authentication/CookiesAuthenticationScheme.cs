using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Claims;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Models;
using Newtonsoft.Json;
using Server.Configuration;
using Server.Models;
using Server.Services.Authorization;
using Server.Services.Background;
using Server.Services.Channels;
using Spotify.API.NetCore.Models;

namespace Server.Services.Authentication
{
  public class CookiesAuthenticationScheme
  {
    public const string Name = CookieAuthenticationDefaults.AuthenticationScheme;

    public const string CookieName = "cpSESSION";
    public const string CookieDomain = ".tunage.app";
    public static TimeSpan CookieMaxAge = TimeSpan.FromHours(12);

    private const string GuestUserJsonClaim = "urn:party:guest:userJson";

    public static void Configure(
      CookieAuthenticationOptions options,
      ITicketStore sessionStore,
      Mode appMode,
      Configuration.Spotify spotifyConfig
    )
    {
      var isStaging = appMode == Mode.Staging;
      var isProduction = appMode == Mode.Production;

      options.SessionStore = sessionStore;
      options.LoginPath = "/auth/login";
      options.LogoutPath = "/auth/logout";
      options.ReturnUrlParameter = "return_to";
      options.ExpireTimeSpan = CookieMaxAge;
      options.SlidingExpiration = true;
      options.Cookie.Name = CookieName;
      options.Cookie.MaxAge = options.ExpireTimeSpan;
      options.Cookie.SecurePolicy = isProduction ? CookieSecurePolicy.Always : CookieSecurePolicy.None;
      options.Cookie.SameSite = SameSiteMode.None;
      options.Cookie.Domain = isProduction || isStaging ? CookieDomain : "";
      options.Cookie.Path = "/";
      options.Cookie.IsEssential = true;

      options.Events = new CookieAuthenticationEvents
      {
        OnRedirectToLogin = context =>
        {
          // Disable direct challenges to this Cookies scheme
          // https://github.com/aspnet/Security/issues/1394
          // https://github.com/aspnet/AspNetCore/blob/62351067ff4c1401556725b401478e648b66acdc/src/Security/Authentication/Cookies/src/CookieAuthenticationEvents.cs#L42
          context.Response.Headers["Location"] = context.RedirectUri;
          context.Response.StatusCode = 401;

          var logger = context.HttpContext.RequestServices
            .GetRequiredService<ILogger<CookiesAuthenticationScheme>>();
          logger.LogDebug("Cookies authentication scheme was challenged with redirect URI \"{0}\"",
            context.RedirectUri);

          return Task.CompletedTask;
        },

        OnSignedIn = async context =>
        {
          bool isUserHost = context.Principal.IsInRole(Roles.Host);
          bool isUserGuest = context.Principal.IsInRole(Roles.Guest);
          if (isUserGuest && !isUserHost) return;

          var dbContext = context.HttpContext.RequestServices.GetRequiredService<PartyModelContainer>();
          var claims = context.Principal.Claims.ToList();
          var claimsByType = claims.ToLookup(claim => claim.Type);
          var token = claimsByType.ToSpotifyToken();
          var tokenScope = claimsByType[SpotifyAuthenticationScheme.SpotifyTokenScopeClaim].First().Value;
          var spotifyUserId = context.Principal.Identity.Name;
          var spotifyPicture = claimsByType[Auth0AuthenticationScheme.Auth0PictureClaim].FirstOrDefault()?.Value;
          var spotifyUserImages = new List<Image>();
          if (spotifyPicture != null)
          {
            spotifyUserImages.Add(new Image
            {
              Url = spotifyPicture
            });
          }
          // TODO: Get at the detailed Auth0 user info
          // The rest of the profile was already fetched beforehand
          var spotifyUser = new PrivateProfile
          {
            Id = spotifyUserId,
            Images = spotifyUserImages
          };

          var logger = context.HttpContext.RequestServices
            .GetRequiredService<ILogger<CookiesAuthenticationScheme>>();
          logger.LogDebug("Cookies authentication authorized with Spotify user {0}",
            JsonConvert.SerializeObject(spotifyUser));

          await SpotifyAuthenticationScheme.UpsertPartyUser(
            dbContext, token, tokenScope, spotifyUser);
        },

        OnValidatePrincipal = async context =>
        {
          bool isUserHost = context.Principal.IsInRole(Roles.Host);
          bool isUserGuest = context.Principal.IsInRole(Roles.Guest);

          // If the user is _just_ a Guest, validate their principal claims
          if (isUserGuest && !isUserHost)
          {
            var guestIsValid = ValidateGuestPrincipal(context);
            if (guestIsValid == false)
            {
              await context.HttpContext.SignOutAsync(Name);
            }
            return;
          }

          // Otherwise, validate a Host's Spotify access token
          var claims = context.Principal.Claims.ToList();
          var claimsByType = claims.ToLookup(claim => claim.Type);

          var spotifyAccessToken = claimsByType.ToSpotifyToken();
          var tokenScope = claimsByType[SpotifyAuthenticationScheme.SpotifyTokenScopeClaim].First().Value;
          var hasRequiredScopes = SpotifyAuthenticationScheme.HasRequiredScopes(tokenScope);
          var rejectHostPrincipal = !hasRequiredScopes;

          // Try to refresh the Host's Spotify access token if it's expired
          if (spotifyAccessToken.IsExpired())
          {
            var spotifyUserId = context.Principal.Identity.Name;
            var updatedPrincipal = await SpotifyAuthenticationScheme
              .RefreshAccessToken(spotifyAccessToken, spotifyUserId, spotifyConfig);

            // If Spotify access token was refreshed replace the Host's principal
            if (updatedPrincipal != null)
            {
              context.ReplacePrincipal(updatedPrincipal);
              context.ShouldRenew = true;
              return;
            }
            // If not, reject the host's principal
            else
            {
              rejectHostPrincipal = true;
            }
          }

          if (rejectHostPrincipal)
          {
            // Reject principal, otherwise
            context.RejectPrincipal();
            context.ShouldRenew = false;

            await context.HttpContext.SignOutAsync(Name);
          }
        }
      };
    }

    public static ClaimsPrincipal CreateGuestPrincipal(Guest guest)
    {
      var claims = new List<Claim>
      {
        new Claim(ClaimsIdentity.DefaultRoleClaimType, Roles.Guest),
        new Claim(ClaimTypes.NameIdentifier, guest.Token),
        new Claim(GuestUserJsonClaim, JsonConvert.SerializeObject(guest)),
      };
      return new ClaimsPrincipal(new ClaimsIdentity(claims, "Guest"));
    }

    public static Guest GetGuest(IEnumerable<Claim> principalClaims)
    {
      if (principalClaims == null) return null;

      var guestJson = principalClaims.ToList()
        .Where(c => c.Type == GuestUserJsonClaim)
        .Select(c => c.Value)
        .FirstOrDefault();
      return string.IsNullOrWhiteSpace(guestJson)
        ? null
        : JsonConvert.DeserializeObject<Guest>(guestJson);
    }

    /// <summary>
    /// Validate a guest user's principal claims
    /// </summary>
    /// <remarks>
    /// Guest sessions are valid for 30 minutes. If their session has less than 15 minutes left
    /// at the time of validation, add 30 minutes to their session.
    ///
    /// Guest sessions are invalid if:
    /// - Their metadata isn't claimed (See <see cref="GetGuest"/>)
    /// - Their session is expired, or
    /// - The current request's Origin doesn't match their own (See <see cref="Guest.OriginMatches"/>)
    /// </remarks>
    /// <param name="context">The principal claims</param>
    /// <returns>True if the guest's session is valid, false otherwise</returns>
    private static bool ValidateGuestPrincipal(CookieValidatePrincipalContext context)
    {
      var guest = GetGuest(context.Principal.Claims);

      var logger = context.HttpContext.RequestServices.GetRequiredService<ILogger<Guest>>();
      logger.LogDebug("Validating guest session {0}", guest.Token);

      // Reject guest principal if it:
      //  - Couldn't be loaded
      //  - Is expired, or
      //  - Origin header is mismatched
      if (guest == null || guest.IsExpired || !guest.OriginMatches(context.Request.Headers))
      {
        context.RejectPrincipal();
        context.ShouldRenew = false;
        logger.LogTrace("Guest session {0} is invalid", guest.Token);

        // TODO: If the guest could be loaded, but is *still* invalid, remove them from their party's guest list with SSE

        return false;
      }

      // Sliding session expiration rules:
      //  - Guest sessions are valid for 30 minutes
      //  - If there's less than 15 minutes left, add 30 minutes to their session
      // TODO: Refactor this to use the `ClaimTypes.Expiration` claim
      if (guest.Expiry.Subtract(DateTime.UtcNow).TotalMinutes >= 15) return true;

      guest.UpdatedAt = DateTime.UtcNow;
      guest.Expiry = guest.UpdatedAt.AddMinutes(30);
      logger.LogTrace("Extending guest session {0} by 30 minutes", guest.Token);

      // Update their party's guest list
      var bg = context.HttpContext.RequestServices.GetRequiredService<IBackgroundTaskQueue>();
      bg.QueueTask(async token =>
      {
        logger.LogTrace("Pushing party updates for guest {0}", guest.Token);
        var db = context.HttpContext.RequestServices.GetRequiredService<PartyModelContainer>();
        var party = await db.Party
          .Include(p => p.Guests)
          .FirstOrDefaultAsync(p => p.Id == guest.PartyId, token);
        if (party == null) return;

        var guests = party.GuestList();
        var existingGuest = guests.FirstOrDefault(g => g.Token == guest.Token);
        if (existingGuest == null) return;

        var existingGuestIndex = guests.IndexOf(existingGuest);
        guests[existingGuestIndex] = guest;
        party.UpdateGuestList(guests);

        await db.SaveChangesAsync(token);

        var partyChannel = context.HttpContext.RequestServices
          .GetRequiredService<IEventChannel<PublicParty>>();
        partyChannel.Push(PublicParty.FromParty(party, guests));
      });

      context.ReplacePrincipal(CreateGuestPrincipal(guest));
      context.ShouldRenew = true;
      logger.LogTrace("Renewing guest session {0}", guest.Token);

      return true;
    }
  }
}
