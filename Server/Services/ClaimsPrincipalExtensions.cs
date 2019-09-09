using System.Linq;
using System.Security.Claims;

namespace Server.Services
{
  public static class ClaimsPrincipalExtensions
  {
    public static string Username(this ClaimsPrincipal principal)
    {
      return principal?.Claims
        .Where(c => c.Type == ClaimTypes.NameIdentifier)
        .Select(c => c.Value)
        .FirstOrDefault();
    }
  }
}
