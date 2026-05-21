using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;

namespace Server.Middleware
{
  /// <summary>
  /// Validate Origin header on WebSocket requests to prevent unexpected cross-site
  /// WebSocket requests.
  /// </summary>
  [UsedImplicitly]
  public class WebSocketOriginPolicyMiddleware
  {
    private readonly RequestDelegate _next;
    private readonly List<string> _allowedOrigins;

    public WebSocketOriginPolicyMiddleware(RequestDelegate next, IEnumerable<string> allowedOrigins)
    {
      _next = next;
      _allowedOrigins = allowedOrigins.ToList();
    }

    [UsedImplicitly]
    public async Task InvokeAsync(HttpContext context)
    {
      // Check for a WebSocket request.
      if (string.Equals(context.Request.Headers["Upgrade"], "websocket"))
      {
        var origin = context.Request.Headers["Origin"];

        // If there is an origin header, and the origin header doesn't match
        // an allowed value:
        if (!string.IsNullOrEmpty(origin) && !_allowedOrigins.Contains(origin))
        {
          // The origin is not allowed, reject the request
          context.Response.StatusCode = StatusCodes.Status403Forbidden;
          return;
        }
      }

      // Call the next delegate/middleware in the pipeline
      await _next(context);
    }
  }

  public static class WebSocketOriginPolicyMiddlewareExtensions
  {
    /// <summary>
    /// Validate Origin header on WebSocket requests to prevent unexpected cross-site
    /// WebSocket requests.
    /// </summary>
    public static IApplicationBuilder UseWebSocketOriginPolicy(
      this IApplicationBuilder builder,
      IEnumerable<string> allowedOrigins
    )
    {
      return builder.UseMiddleware<WebSocketOriginPolicyMiddleware>(allowedOrigins);
    }
  }
}
