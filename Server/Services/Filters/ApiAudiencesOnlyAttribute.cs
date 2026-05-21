using System;
using Microsoft.AspNetCore.Authorization;
using Server.Services.Authentication;

namespace Server.Services.Filters
{
  [AttributeUsage(
    AttributeTargets.Class | AttributeTargets.Method,
    Inherited = true, AllowMultiple = false)]
  public class AuthorizeForApiAudiencesAttribute : AuthorizeAttribute
  {
    /// <summary>
    /// Requires that applicable requests are authorized with an Auth0 JWT.
    /// </summary>
    public AuthorizeForApiAudiencesAttribute()
    {
      AuthenticationSchemes = Auth0AuthenticationScheme.NameJwt;
      Roles = Authorization.Roles.Guest;
    }
  }
}
