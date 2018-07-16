using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Internal;
using Models;
using Nancy;
using Nancy.Bootstrapper;
using Nancy.SimpleAuthentication;
using Nancy.TinyIoc;
using Server.Configuration;
using Server.Services.Auth;
using SimpleAuthentication.Core;

namespace Server
{
  public class PartyBootstrapper : DefaultNancyBootstrapper
  {
    private readonly AppConfiguration _appConfig;
    private readonly DbContextPool<PartyModelContainer> _dbContextPool;

    private readonly AuthenticationProviderFactory _authenticationSchemes;
    private readonly SpotifyProvider _spotifyProvider;

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

      _spotifyProvider = new SpotifyProvider(new ProviderParams()
      {
        PublicApiKey = appConfig.Spotify.AppKey,
        SecretApiKey = appConfig.Spotify.AppSecret,
        Scopes = SpotifyProvider.PartySpotifyScopes
      });

      _authenticationSchemes = new AuthenticationProviderFactory();
      _authenticationSchemes.AddProvider(_spotifyProvider);
    }

    protected override void ConfigureApplicationContainer(TinyIoCContainer container)
    {
      base.ConfigureApplicationContainer(container);

      container.Register(_appConfig);
      container.Register<IAuthenticationCallbackProvider>(new SpotifyCallbackProvider(_dbContextPool));
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
