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

namespace Server.Services.Authentication
{
  class Auth0AuthenticationScheme
  {
    public const string Name = "Auth0";
    public const string SpotifyConnection = "Spotify-Tunage";
    public const string Auth0NameClaim = "name";
    public const string Auth0NicknameClaim = "nickname";
    public const string Auth0PictureClaim = "picture";
    private const string Callback = "/auth/zero/callback";

    public static void ConfigureJwtBearer(JwtBearerOptions options, Configuration.Auth0 auth0Config)
    {
      options.Authority = auth0Config.Domain;
      options.Audience = auth0Config.Audience;
      options.TokenValidationParameters = new TokenValidationParameters
      {
        RequireSignedTokens = true,
        NameClaimType = ClaimTypes.NameIdentifier
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
          context.ProtocolMessage.SetParameter("audience", auth0Config.Audience);
          var connection = context.Properties.GetString("connection") ?? SpotifyConnection;
          context.ProtocolMessage.SetParameter("connection", connection);
          return Task.CompletedTask;
        },
        OnTicketReceived = context =>
        {
          // https://auth0.com/docs/architecture-scenarios/web-app-sso/implementation-aspnetcore#implement-admin-permissions
          var options = context.Options as OpenIdConnectOptions;

          var identity = context.Principal.Identity as ClaimsIdentity;
          if (identity != null)
          {
            // Replace the Auth0's default Name claim with the nickname claim
            // I have an Auth0 rule setup to set the Spotify username
            var nameClaim =
              identity.Claims.FirstOrDefault(claim => claim.Type == ClaimTypes.NameIdentifier);
            if (nameClaim != null && identity.HasClaim(claim => claim == nameClaim))
            {
              identity.RemoveClaim(nameClaim);
            }
            var nickname =
              identity.Claims.FirstOrDefault(claim => claim.Type == Auth0NicknameClaim).Value;
            identity.AddClaim(new Claim(ClaimTypes.NameIdentifier, nickname));
            // Convert the idToken's claimed roles to claims understood by ASP.NET
            var claimedRoles = context.Principal
              .FindAll(c => c.Type == "https://api.tunage.app/roles")
              .Select(claim => claim.Value)
              .Where(role => Roles.All.Contains(role))
              .ToList();
            foreach (var role in claimedRoles)
            {
              identity.AddClaim(new Claim(
                ClaimTypes.Role, role,
                ClaimValueTypes.String,
                options.Authority
              ));
            }
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
  }
}
