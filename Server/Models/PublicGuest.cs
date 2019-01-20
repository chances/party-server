using Newtonsoft.Json;

namespace Server.Models
{
  public class PublicGuest
  {
    [JsonProperty("name")]
    public string Name { get; set; }

    [JsonProperty("alias")]
    public string Alias { get; set; }

    [JsonProperty("checked_in")]
    public bool CheckedIn { get; set; }
  }
}
