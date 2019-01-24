using System;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using Server.Services.Authentication;

namespace Server.Services.Filters
{
  [AttributeUsage(AttributeTargets.Method)]
  public class UserAuthenticatedAttribute : ActionFilterAttribute
  {
    public override void OnActionExecuting(ActionExecutingContext context)
    {
      var isHostUser = context.HttpContext.User?.IsInRole(Roles.Host) ?? false;
      var isGuestUser = context.HttpContext.User?.IsInRole(Roles.Guest) ?? false;
      var authenticated = isHostUser || isGuestUser;
      if (!authenticated)
      {
        context.Result = new UnauthorizedResult();
      }
    }
  }
}
