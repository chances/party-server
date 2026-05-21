using Newtonsoft.Json;

namespace Server.Models
{
  public class PublicGuest
  {
    public PublicGuest()
    {
    }

    public PublicGuest(PublicGuest guest)
    {
      Name = guest.Name;
      Alias = guest.Alias;
      CheckedIn = guest.CheckedIn;
    }

    [JsonProperty("name")]
    public string Name { get; set; }

    [JsonProperty("alias")]
    public string Alias { get; set; }

    [JsonProperty("checked_in")]
    public bool CheckedIn { get; set; }
  }
}
