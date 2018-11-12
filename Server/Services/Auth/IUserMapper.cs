using System.Security.Claims;

namespace Server.Services.Auth
{
  public interface IUserMapper
  {
    ClaimsPrincipal GetUserFromSession();
  }
}
