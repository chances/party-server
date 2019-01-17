using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Models;
using Server.Configuration;

namespace Server
{
  public class Startup
  {
    private readonly AppConfiguration _appConfig = new AppConfiguration();

    // This method gets called by the runtime. Use this method to add services to the container.
    // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
    public void ConfigureServices(IServiceCollection services)
    {
      services.AddDbContextPool<PartyModelContainer>(options => options.UseNpgsql(_appConfig.ConnectionString), 32);
      services.AddMvc();
    }

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    public void Configure(IApplicationBuilder app, IHostingEnvironment env)
    {
      if (_appConfig.Mode == Mode.Development)
      {
        app.UseDeveloperExceptionPage();
      }

      app.UseStaticFiles();
      app.UseMvc();
    }
  }
}
