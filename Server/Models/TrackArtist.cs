using Newtonsoft.Json;

namespace Server.Models
{
  public class TrackArtist
  {
    [JsonProperty("id")]
    public string Id { get; set; }

    [JsonProperty("name")]
    public string Name { get; set; }
  }
}
