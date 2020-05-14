using System;
using Newtonsoft.Json;

namespace Server.Models
{
  public class PlayingTrack : Track
  {
    [JsonProperty("paused")]
    public bool Paused { get; set; }

    [JsonProperty("elapsed")]
    public uint Elapsed { get; set; }
  }
}
