using System;
using System.Globalization;
using System.Linq;
using System.Security.Claims;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.AspNetCore.Authentication.OpenIdConnect;
using Microsoft.AspNetCore.Http;
using Microsoft.IdentityModel.Protocols.OpenIdConnect;
using Microsoft.IdentityModel.Tokens;
using Http = Microsoft.AspNetCore.Http;
using Server.Services.Authorization;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace Server.Services.Authentication
{
  class Auth0AuthenticationScheme
  {
    public const string NameJwt = "Jwt";
    public const string NameAuth0 = "Auth0";
    public const string SpotifyConnection = "Spotify-Tunage";
    public const string Auth0NameClaim = "name";
    public const string Auth0NicknameClaim = "nickname";
    public const string Auth0PictureClaim = "picture";
    private const string Callback = "/auth/zero/callback";

    public static void ConfigureJwtBearer(JwtBearerOptions options, Configuration.Auth0 auth0Config)
    {
      options.Authority = auth0Config.Domain;
      options.Challenge = "Bearer realm=\"tunage\", error=\"unauthorized\", error_description=\"Unauthorized request made to Tunage API\"";
      options.TokenValidationParameters = new TokenValidationParameters
      {
        RequireSignedTokens = true,
        ValidAudiences = new string[]
        {
          auth0Config.ClientId,
          auth0Config.Audience
        },
        NameClaimType = ClaimTypes.NameIdentifier
      };

      options.Events = new JwtBearerEvents
      {
        OnAuthenticationFailed = context =>
        {
          var logMessage = "JWT Bearer token authentication failed";
          Sentry.SentrySdk.AddBreadcrumb(logMessage, "auth",
            level: Sentry.Protocol.BreadcrumbLevel.Warning);
          var logger = context.HttpContext.RequestServices
            .GetRequiredService<ILogger<Auth0AuthenticationScheme>>();
          logger.LogWarning(logMessage);

          return Task.CompletedTask;
        },
        OnTokenValidated = context =>
        {
          Sentry.SentrySdk.AddBreadcrumb(
            "JWT Bearer token validated", "auth",
            level: Sentry.Protocol.BreadcrumbLevel.Info);

          var identity = context.Principal.Identity as ClaimsIdentity;
          ReplaceNameAndRoleClaims(identity, options.Authority);

          var logger = context.HttpContext.RequestServices
            .GetRequiredService<ILogger<Auth0AuthenticationScheme>>();
          if (logger.IsEnabled(LogLevel.Debug))
          {
            logger.LogDebug("JWT Bearer principal claims have been augmented with name '{0}' and roles {1}",
              identity.Name,
              identity.Claims.Where(claim => claim.Type == ClaimTypes.Role)
              .Select(claim => claim.Value)
            );
          }

          // TODO: Add Guest identity validation when clients support JWT tokens

          return Task.CompletedTask;
        }
      };
    }

    public static void ConfigureOidc(OpenIdConnectOptions options, Configuration.Auth0 auth0Config)
    {
      options.Authority = auth0Config.Domain;
      options.ClaimsIssuer = "Auth0";
      options.ResponseType = OpenIdConnectResponseType.Code;
      options.SaveTokens = true;
      options.GetClaimsFromUserInfoEndpoint = true;
      options.TokenValidationParameters = new TokenValidationParameters
      {
        NameClaimType = ClaimTypes.NameIdentifier
      };

      options.ClientId = auth0Config.ClientId;
      options.ClientSecret = auth0Config.ClientSecret;
      options.CallbackPath = new Http.PathString(Callback);

      var scopes = new string[] { "openid", "name", "host", "guest" };
      scopes.ToList().ForEach(options.Scope.Add);

      options.Events = new OpenIdConnectEvents
      {
        OnRedirectToIdentityProvider = context =>
        {
          var logMessage = "Auth0 authentication scheme challenged";
          Sentry.SentrySdk.AddBreadcrumb(logMessage, "auth",
            level: Sentry.Protocol.BreadcrumbLevel.Debug);
          var logger = context.HttpContext.RequestServices
            .GetRequiredService<ILogger<Auth0AuthenticationScheme>>();
          logger.LogDebug(logMessage);

          // If this is an API request, don't redirect to Auth0
          // https://github.com/auth0-samples/aspnet-core-mvc-plus-webapi/tree/73d2015e82f80f898d012fa14cb8ba7022432f16#1-add-the-cookie-and-oidc-middleware
          if (IsAjaxRequest(context.Request) || IsApiRequest(context.Request))
          {
            logMessage = "Challenging Auth0 JWT authentication scheme instead";
            Sentry.SentrySdk.AddBreadcrumb(logMessage, "auth",
              level: Sentry.Protocol.BreadcrumbLevel.Debug);
            logger.LogDebug(logMessage);

            context.HandleResponse();
            return context.HttpContext.ChallengeAsync(NameJwt);
          }

          // Otherwise, challenge Auth0
          context.ProtocolMessage.SetParameter("audience", auth0Config.Audience);
          var connection = context.Properties.GetString("connection") ?? SpotifyConnection;
          context.ProtocolMessage.SetParameter("connection", connection);
          return Task.CompletedTask;
        },
        OnTicketReceived = context =>
        {
          var identity = context.Principal.Identity as ClaimsIdentity;
          ReplaceNameAndRoleClaims(identity, options.Authority);

          var logger = context.HttpContext.RequestServices
            .GetRequiredService<ILogger<Auth0AuthenticationScheme>>();
          if (logger.IsEnabled(LogLevel.Debug))
          {
            logger.LogDebug("Auth0 cookie's principal claims have been augmented with name '{0}' and roles {1}",
              identity.Name,
              identity.Claims.Where(claim => claim.Type == ClaimTypes.Role)
              .Select(claim => claim.Value)
            );
          }

          return Task.CompletedTask;
        },
        OnUserInformationReceived = context =>
        {
          // TODO: Something with `context.User`?

          return Task.CompletedTask;
        },
        OnRedirectToIdentityProviderForSignOut = context =>
        {
          var logoutUri = $"{auth0Config.Domain}/v2/logout?client_id={auth0Config.ClientId}";

          // Handle logout RedirectUri
          var redirectUri = context.Properties.RedirectUri;
          if (!string.IsNullOrEmpty(redirectUri))
          {
            if (redirectUri.StartsWith("/"))
            {
              // Make URI absolute
              var request = context.Request;
              redirectUri = request.Scheme + "://" + request.Host + request.PathBase + redirectUri;
            }
            logoutUri += $"&returnTo={ Uri.EscapeDataString(redirectUri)}";
          }

          context.Response.Redirect(logoutUri);
          context.HandleResponse();

          return Task.CompletedTask;
        }
      };
    }

    public static async Task<bool> IsAccessTokenExpiredAsync(HttpContext context)
    {
      // https://auth0.com/docs/quickstart/webapp/aspnet-core-3#store-the-tokens
      var accessTokenExpiresAt = DateTime.Parse(
        await context.GetTokenAsync("expires_at"),
        CultureInfo.InvariantCulture,
        DateTimeStyles.RoundtripKind
      );
      return accessTokenExpiresAt <= DateTime.UtcNow;
    }

    private static bool IsAjaxRequest(HttpRequest request)
    {
      // Adapted from https://github.com/dotnet/aspnetcore/blob/62351067ff4c1401556725b401478e648b66acdc/src/Security/Authentication/Cookies/src/CookieAuthenticationEvents.cs#L103
      return string.Equals(request.Query["X-Requested-With"].ToString().Trim(), "XMLHttpRequest", StringComparison.Ordinal) ||
        string.Equals(request.Headers["X-Requested-With"].ToString().Trim(), "XMLHttpRequest", StringComparison.Ordinal);
    }

    private static bool IsApiRequest(HttpRequest request)
    {
      // Adapted from https://github.com/auth0-samples/aspnet-core-mvc-plus-webapi/blob/73d2015e82f80f898d012fa14cb8ba7022432f16/SampleMvcApp/Startup.cs#L143
      var path = request.Path.HasValue ? request.Path : new PathString("/");
      var isHomePage = string.Equals(request.Path.Value.Trim(), "/", StringComparison.Ordinal);
      return !isHomePage;
    }

    private static void ReplaceNameAndRoleClaims(ClaimsIdentity identity, string authority)
    {
      // Adapted from https://auth0.com/docs/architecture-scenarios/web-app-sso/implementation-aspnetcore#implement-admin-permissions
      // Replace the Auth0's default Name claim with the nickname claim
      // I have an Auth0 rule setup to set the Spotify username
      var claimsByType = identity.Claims.ToLookup(claim => claim.Type);
      string nickname = null;
      var nameClaim = claimsByType[ClaimTypes.NameIdentifier]?.FirstOrDefault() ?? null;
      if (nameClaim?.Value.StartsWith("oauth2|Spotify-Tunage|") ?? false)
      {
        var maybeSpotifyUsername = nameClaim.Value.Split("oauth2|Spotify-Tunage|");
        nickname = maybeSpotifyUsername.Length > 1 ? maybeSpotifyUsername[1] ?? null : null;
      }
      var nicknameClaimValue = claimsByType[Auth0NicknameClaim]?.FirstOrDefault()?.Value ?? null;
      identity.TryRemoveClaim(nameClaim);
      nickname = nicknameClaimValue ?? nickname;
      if (nickname != null) identity.AddClaim(new Claim(ClaimTypes.NameIdentifier, nickname));
      // Convert the Auth0 claimed roles to claims understood by ASP.NET
      var claimedRoles = identity.Claims
        .Where(c => c.Type == "https://api.tunage.app/roles")
        .Select(claim => claim.Value)
        .Where(role => Roles.All.Contains(role))
        .ToList();
      foreach (var role in claimedRoles)
      {
        identity.AddClaim(new Claim(ClaimTypes.Role, role, ClaimValueTypes.String, authority));
      }
    }
  }
}
