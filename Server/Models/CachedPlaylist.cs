using System.Collections.Generic;
using Newtonsoft.Json;

namespace Server.Models
{
  public class CachedPlaylist
  {
    [JsonProperty("playlist")]
    public Playlist Playlist { get; set; }

    [JsonProperty("tracks")]
    public List<Track> Tracks { get; set; }
  }
}
