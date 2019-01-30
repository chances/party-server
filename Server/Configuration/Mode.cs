namespace Server.Configuration
{
  public enum Mode
  {
    Development,
    Staging,
    Production
  }

  public static class ModeExtensions
  {
    public static bool IsProduction(this Mode mode)
    {
      return mode == Mode.Production;
    }

    public static bool IsDevelopment(this Mode mode)
    {
      return mode == Mode.Development;
    }
  }
}
