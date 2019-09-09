using System;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using Microsoft.Net.Http.Headers;
using Server.Services.Authentication;

namespace Server.Services.Filters
{
  /// <summary>
  /// Filter requests to a Party API endpoint to those who are authenticated.
  /// </summary>
  [AttributeUsage(AttributeTargets.Method)]
  public class UserAuthenticatedAttribute : ActionFilterAttribute
  {
    public override void OnActionExecuting(ActionExecutingContext context)
    {
      var isHostUser = context.HttpContext.User?.IsInRole(Roles.Host) ?? false;
      var isGuestUser = context.HttpContext.User?.IsInRole(Roles.Guest) ?? false;
      var authenticated = isHostUser || isGuestUser;
      if (authenticated) return;

      context.Result = new UnauthorizedResult();
      context.HttpContext.Response.Headers.Add(
        HeaderNames.WWWAuthenticate,
        "Bearer realm=\"spotify\", error=\"unauthorized\", error_description=\"Unauthorized request made to Party\""
      );
    }
  }
}
