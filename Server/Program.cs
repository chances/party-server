﻿using System;
using System.IO;
using JetBrains.Annotations;
using Microsoft.AspNetCore.Hosting;

namespace Server
{
  [UsedImplicitly]
  public class Program
  {
    public static void Main(string[] args)
    {
      var port = Environment.GetEnvironmentVariable("PORT");
      if (string.IsNullOrWhiteSpace(port))
      {
        port = "3005";
      }

      var host = new WebHostBuilder()
        .UseContentRoot(Directory.GetCurrentDirectory())
        .UseUrls($"http://localhost:{port}")
        .UseKestrel()
        .UseStartup<Startup>()
        .Build();

      host.Run();
    }
  }
}
