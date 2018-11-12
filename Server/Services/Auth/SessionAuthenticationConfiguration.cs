using System.Security.Claims;
using JetBrains.Annotations;
using Nancy;
using Nancy.Authentication.Stateless;
using Nancy.Bootstrapper;

namespace Server.Services.Auth
{
  [UsedImplicitly]
  public class SessionAuthenticationConfiguration : StatelessAuthenticationConfiguration//, IRequestStartup
  {
    public SessionAuthenticationConfiguration(IUserMapper userMapper) :
      base(context => AuthenticateSession(userMapper, context))
    {
    }

    // ReSharper disable once UnusedParameter.Local
    private static ClaimsPrincipal AuthenticateSession(IUserMapper userMapper, NancyContext _) =>
      userMapper.GetUserFromSession();

    public void Initialize(IPipelines pipelines, NancyContext context)
    {
      StatelessAuthentication.Enable(pipelines, this);
    }
  }
}
