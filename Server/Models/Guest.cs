using System;
using Microsoft.AspNetCore.Http;
using Newtonsoft.Json;

namespace Server.Models
{
  public class Guest : PublicGuest
  {
    [JsonProperty("token")]
    public string Token { get; set; }

    [JsonProperty("origin")]
    public string Origin { get; set; }

    [JsonProperty("expiry")]
    public DateTime Expiry { get; set; }

    [JsonIgnore] public bool IsExpired => Expiry <= DateTime.UtcNow;

    [JsonProperty("updated_at")]
    public DateTime UpdatedAt { get; set; }

    [JsonProperty("party_id")]
    public int PartyId { get; set; }

    public Guest()
    {
      Token = Guid.NewGuid().ToString();
      Expiry = DateTime.UtcNow.AddMinutes(30);
      UpdatedAt = DateTime.UtcNow;
    }

    public bool OriginMatches(IHeaderDictionary headers) =>
      headers.ContainsKey("Origin") && headers["Origin"] == Origin;
  }
}
