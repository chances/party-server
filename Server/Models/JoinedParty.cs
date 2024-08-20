using Newtonsoft.Json;

namespace Server.Models
{
    public class JoinedParty
    {
        [JsonProperty("guest_index", NullValueHandling = NullValueHandling.Include)]
        public int GuestIndex { get; set; }
        [JsonProperty("party", NullValueHandling = NullValueHandling.Include)]
        public PublicParty Party { get; set; }
    }
}
