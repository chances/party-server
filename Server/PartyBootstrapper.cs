using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Internal;
using Microsoft.Extensions.Caching.Redis;
using Microsoft.Extensions.Options;
using Models;
using Nancy;
using Nancy.Bootstrapper;
using Nancy.Configuration;
using Nancy.TinyIoc;
using Server.Configuration;
using Server.Services.Cache;
using Server.Services.Repositories;

namespace Server
{
  public class PartyBootstrapper : DefaultNancyBootstrapper
  {
    private readonly AppConfiguration _appConfig;
    private readonly RedisCache _redisCache;
    private readonly DbContextPool<PartyModelContainer> _dbContextPool;

    public PartyBootstrapper(AppConfiguration appConfig)
    {
      _appConfig = appConfig;
      _redisCache = new RedisCache(new OptionsWrapper<RedisCacheOptions>(new RedisCacheOptions()
      {
        Configuration = appConfig.RedisUrl.ToRedisConnectionString()
      }));
      _dbContextPool =
        new DbContextPool<PartyModelContainer>(
          new DbContextOptionsBuilder().UseNpgsql(_appConfig.ConnectionString).Options
        );
    }

    public override void Configure(INancyEnvironment environment)
    {
      base.Configure(environment);

      if (_appConfig.Mode.IsDevelopment())
      {
        environment.Tracing(enabled: false, displayErrorTraces: true);
      }

      environment.AddValue(_appConfig.Mode);
    }

    protected override void ConfigureApplicationContainer(TinyIoCContainer container)
    {
      base.ConfigureApplicationContainer(container);

      container.Register(_appConfig);
      container.Register(_redisCache);
    }

    protected override void ConfigureRequestContainer(TinyIoCContainer container, NancyContext context)
    {
      base.ConfigureRequestContainer(container, context);

      container.Register((c, p) => _dbContextPool.Rent());
      container.Register<IUserRepository, UserRepository>();
    }

    protected override void RequestStartup(TinyIoCContainer container, IPipelines pipelines, NancyContext context)
    {
      base.RequestStartup(container, pipelines, context);

      pipelines.AfterRequest.AddItemToEndOfPipeline(nancyContext =>
      {
        var dbContext = container.Resolve<PartyModelContainer>();
        _dbContextPool.Return(dbContext);
      });
    }

    protected override void Dispose(bool disposing)
    {
      _dbContextPool.Dispose();

      base.Dispose(disposing);
    }
  }
}
