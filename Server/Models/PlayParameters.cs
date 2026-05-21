using Server.Data;

namespace Server.Models
{
  [ResourceIdentifier("play")]
  public class PlayParameters
  {
    public static readonly PlayParameters Default = new PlayParameters
    {
      Shuffle = false
    };

    public bool Shuffle { get; set; }
  }
}
