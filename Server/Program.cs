﻿using System;
using JetBrains.Annotations;
using Microsoft.AspNetCore;
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

      BuildWebHost(args, port).Run();
    }

    private static IWebHost BuildWebHost(string[] args, string port) =>
      WebHost.CreateDefaultBuilder(args)
        .UseUrls($"http://*:{port}")
        .UseWebRoot("./Server/public")
        .UseKestrel()
        .UseStartup<Startup>()
        .UseSentry()
        .Build();
  }
}
