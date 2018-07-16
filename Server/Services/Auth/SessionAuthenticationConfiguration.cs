using System.Security.Claims;
using Nancy;
using Nancy.Authentication.Stateless;

namespace Server.Services.Auth
{
  public class SessionAuthenticationConfiguration : StatelessAuthenticationConfiguration
  {
    public SessionAuthenticationConfiguration(IUserMapper userMapper) :
      base(context => AuthenticateSession(userMapper, context))
    {
    }

    // ReSharper disable once UnusedParameter.Local
    private static ClaimsPrincipal AuthenticateSession(IUserMapper userMapper, NancyContext _) =>
      userMapper.GetUserFromSession();
  }
}
