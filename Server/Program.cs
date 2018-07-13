using System;
using System.IO;
using Microsoft.AspNetCore.Hosting;

namespace Server
{
  public class Program
  {
    public static void Main(string[] args)
    {
      var host = new WebHostBuilder()
        .UseContentRoot(Directory.GetCurrentDirectory())
        .UseKestrel()
        .UseStartup<Startup>()
        .Build();
      
      host.Run();
    }
  }
}
