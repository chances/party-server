using System;
using Microsoft.AspNetCore.Http;

namespace Server.Services
{
  public class ScopedService
  {
    private readonly IHttpContextAccessor _context;

    public ScopedService(IHttpContextAccessor context)
    {
      _context = context;
    }

    protected HttpContext HttpContext => _context.HttpContext;
  }
}
