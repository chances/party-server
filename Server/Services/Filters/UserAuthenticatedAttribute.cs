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
      var authenticated = context.HttpContext.User?.IsInRole(Roles.Authenticated) ?? false;
      if (!authenticated)
      {
        context.Result = new UnauthorizedResult();
      }
    }
  }
}
