using JetBrains.Annotations;
using Nancy;
using Nancy.Bootstrapper;
using Nancy.Configuration;
using Nancy.Responses;
using Server.Configuration;

namespace Server.Services
{
  [UsedImplicitly]
  public class EnsureSsl : IRequestStartup
  {
    private readonly Mode _mode;

    public EnsureSsl(INancyEnvironment environment)
    {
      _mode = environment.GetValue<Mode>();
    }

    public void Initialize(IPipelines pipelines, NancyContext context)
    {
      pipelines.BeforeRequest.AddItemToStartOfPipeline(RedirectForSsl);
    }

    private Response RedirectForSsl(NancyContext context)
    {
      var url = context.Request.Url;
      if (_mode.IsDevelopment() || url.IsSecure) return context.GetResponse();

      url.Scheme = "https";
      return new RedirectResponse(url.ToString());
    }
  }
}
