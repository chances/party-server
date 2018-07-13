using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Internal;
using Models;
using Nancy;
using Nancy.Bootstrapper;
using Nancy.TinyIoc;
using Server.Configuration;

namespace Server
{
  public class PartyBootstrapper : DefaultNancyBootstrapper
  {
    private readonly AppConfiguration _appConfig;
    private readonly DbContextPool<PartyModelContainer> _dbContextPool;

    public PartyBootstrapper()
    {
    }

    public PartyBootstrapper(AppConfiguration appConfig)
    {
      _appConfig = appConfig;
      _dbContextPool =
        new DbContextPool<PartyModelContainer>(
          new DbContextOptionsBuilder().UseNpgsql(_appConfig.ConnectionString).Options
        );
    }

    protected override void ConfigureApplicationContainer(TinyIoCContainer container)
    {
      base.ConfigureApplicationContainer(container);

      container.Register(_appConfig);
    }

    protected override void RequestStartup(TinyIoCContainer container, IPipelines pipelines, NancyContext context)
    {
      base.RequestStartup(container, pipelines, context);

      var dbContext = _dbContextPool.Rent();
      container.Register((_, __) => dbContext);

      pipelines.AfterRequest += (afterContext) =>
      {
        _dbContextPool.Return(dbContext);
      };
    }

    protected override void Dispose(bool disposing)
    {
      _dbContextPool.Dispose();

      base.Dispose(disposing);
    }
  }
}
