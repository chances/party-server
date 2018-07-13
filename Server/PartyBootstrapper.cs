using Nancy;
using Nancy.TinyIoc;

namespace Server
{
  public class PartyBootstrapper : DefaultNancyBootstrapper
  {
    private readonly AppConfiguration _appConfig;
    
    public PartyBootstrapper()
    {
    }

    public PartyBootstrapper(AppConfiguration appConfig)
    {
      _appConfig = appConfig;
    }

    protected override void ConfigureApplicationContainer(TinyIoCContainer container)
    {
      base.ConfigureApplicationContainer(container);

      container.Register(_appConfig);
    }
  }
}
