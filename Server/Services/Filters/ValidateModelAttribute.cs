using System;
using Microsoft.AspNetCore.Mvc.Filters;
using Server.Data;

namespace Server.Services.Filters
{
  /// <summary>
  /// Validate an annotated action's model input for Party API endpoints
  /// </summary>
  [AttributeUsage(AttributeTargets.Method)]
  public class ValidateModelAttribute : ActionFilterAttribute
  {
    public override void OnActionExecuting(ActionExecutingContext context)
    {
      if (!context.ModelState.IsValid)
      {
        context.Result = Error.BadRequest(context.ModelState.Errors());
      }
    }
  }
}
