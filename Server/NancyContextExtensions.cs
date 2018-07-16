using LiteGuard;
using Nancy;

namespace Server
{
  public static class NancyContextExtensions
  {
    public static Response GetResponse(this NancyContext context)
    {
      Guard.AgainstNullArgument(nameof(context), context);

      return context.Response ?? new Response();
    }
  }
}
