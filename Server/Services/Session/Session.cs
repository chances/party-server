using System;
using System.Collections.Generic;
using JetBrains.Annotations;
using Nancy;
using Nancy.Bootstrapper;
using Nancy.Configuration;
using Nancy.Cookies;
using Server.Configuration;

namespace Server.Services.Session
{
  [UsedImplicitly]
  public class Session : IRequestStartup
  {
    private const string AuthStateSessionKey = "AUTH_STATE";
    private const string UserSessionKey = "USER";

    private const string SessionName = "cpSESSION";
    private static readonly TimeSpan MaxAge = TimeSpan.FromHours(12);
    private readonly Mode _appMode;
    private readonly SessionStore _store;

    public Session(INancyEnvironment environment, SessionStore store)
    {
      _appMode = environment.GetValue<Mode>();
      _store = store;
    }

    public void Initialize(IPipelines pipelines, NancyContext context)
    {
      pipelines.BeforeRequest.AddItemToStartOfPipeline(LoadSession);
      pipelines.AfterRequest.AddItemToEndOfPipeline(SaveSession);
    }

    public Guid Id { get; private set; }
    public Dictionary<string, string> Flashes { get; private set; }
    public string AuthState
    {
      get => _store.Get(AuthStateSessionKey);
      set
      {
        if (value == null)
        {
          _store.Delete(AuthStateSessionKey);
        }
        else
        {
          _store.Set(AuthStateSessionKey, value);
        }
      }
    }

    public string Error => Flashes.ContainsKey("error") ? Flashes["error"] : null;

    public string Username
    {
      get => _store.Get(UserSessionKey);
      set => _store.Set(UserSessionKey, value);
    }

    public void Flash(string key, string value) => Flashes.Add(key, value);

    public void Logout() => _store.Delete(UserSessionKey);

    private void ResetSession()
    {
      Id = Guid.NewGuid();
      Flashes = new Dictionary<string, string>();
    }

    private Response LoadSession(NancyContext context)
    {
      if (context.Request.Cookies.ContainsKey(SessionName))
      {
        var sessionId = context.Request.Cookies[SessionName];
        Flashes = _store.LoadFlashes(Id = Guid.Parse(sessionId));
        return context.GetResponse();
      }
      else
      {
        ResetSession();
      }

      var cookie = new NancyCookie(
        SessionName,
        Id.ToString(),
        true, // HTTPOnly
        _appMode.IsProduction(), // Secure
        DateTime.UtcNow.Add(MaxAge)
      )
      {
        Domain = _appMode.IsDevelopment() ? null : ".chancesnow.me"
      };
      return context.GetResponse().WithCookie(cookie);
    }

    private void SaveSession(NancyContext context)
    {
      _store.SaveFlashes(Id, Flashes);
    }
  }
}
