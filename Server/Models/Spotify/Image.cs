using Newtonsoft.Json;

namespace Server.Models.Spotify
{
  public class Image
  {
    public Image()
    {
    }

    [JsonProperty("url")]
    public string Url { get; set; }
    [JsonProperty("width", NullValueHandling = NullValueHandling.Ignore)]
    public int? Width { get; set; }
    [JsonProperty("height", NullValueHandling = NullValueHandling.Ignore)]
    public int? Height { get; set; }
  }
}
