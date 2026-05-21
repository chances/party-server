using System.Collections.Generic;
using Newtonsoft.Json;
using Server.Data;

namespace Server.Models
{
  public class Queue
  {
    [JsonProperty("tracks", NullValueHandling = NullValueHandling.Include)]
    public IList<Track> Tracks { get; set; }
  }
}
