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
  public class Session : ISession, IRequestStartup
  {
    private const string AuthStateSessionKey = "AUTH_STATE";
    private const string UserSessionKey = "USER";

    private const string SessionName = "cpSESSION";
    private static readonly TimeSpan MaxAge = TimeSpan.FromHours(12);
    private readonly Mode _appMode;
    private readonly SessionStore _store;

    private Guid _id;
    private Dictionary<string, string> _flashes;
    private bool _isFresh = true;

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

    public string Error => _flashes.ContainsKey("error") ? _flashes["error"] : null;

    public string Username
    {
      get => _store.Get($"{_id}:{UserSessionKey}");
      set => _store.Set($"{_id}:{UserSessionKey}", value);
    }

    public void Flash(string key, string value) => _flashes.Add(key, value);

    public void Logout() => _store.Delete($"{_id}:{UserSessionKey}");

    private void ResetSession()
    {
      _id = Guid.NewGuid();
      _flashes = new Dictionary<string, string>();
      _isFresh = true;
    }

    private Response LoadSession(NancyContext context)
    {
      if (context.Request.Cookies.ContainsKey(SessionName))
      {
        var sessionId = context.Request.Cookies[SessionName];
        _flashes = _store.LoadFlashes(_id = Guid.Parse(sessionId));
        _isFresh = false;
      }
      else
      {
        ResetSession();
      }

      return null;
    }

    private void SaveSession(NancyContext context)
    {
      _store.SaveFlashes(_id, _flashes);

      // Only set the cookie if the session is "fresh", i.e. newly created
      if (!_isFresh) return;

      var cookie = new NancyCookie(
        SessionName,
        _id.ToString(),
        true, // HTTPOnly
        _appMode.IsProduction(), // Secure
        DateTime.UtcNow.Add(MaxAge)
      )
      {
        Domain = _appMode.IsDevelopment() ? null : ".chancesnow.me"
      };
      context.Response.Cookies.Add(cookie);
    }
  }
}
