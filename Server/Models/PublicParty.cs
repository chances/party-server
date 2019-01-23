using System;
using System.Collections.Generic;
using System.Linq;
using Models;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Server.Models
{
  public class PublicParty
  {
    [JsonProperty("location", NullValueHandling = NullValueHandling.Include)]
    public JObject Location { get; set; }

    [JsonProperty("room_code", NullValueHandling = NullValueHandling.Include)]
    public string RoomCode { get; set; }

    [JsonProperty("ended", NullValueHandling = NullValueHandling.Include)]
    public bool Ended { get; set; }

    [JsonProperty("guests", NullValueHandling = NullValueHandling.Ignore)]
    public List<PublicGuest> Guests { get; set; }

    [JsonProperty("current_track", NullValueHandling = NullValueHandling.Ignore)]
    public PlayingTrack CurrentTrack { get; set; }

    public static PublicParty FromParty(Party party) => new PublicParty {
      Location = string.IsNullOrWhiteSpace(party.Location)
        ? null
        : JsonConvert.DeserializeObject<JObject>(party.Location),
      RoomCode = party.RoomCode,
      Ended = party.Ended,
      Guests = party.GuestList().Cast<PublicGuest>().ToList(),
      CurrentTrack = string.IsNullOrWhiteSpace(party.CurrentTrack)
        ? null
        : JsonConvert.DeserializeObject<PlayingTrack>(party.CurrentTrack)
    };
  }
}