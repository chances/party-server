using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Server.Data
{
  public class Resource : ResourceIdentifier, IResource
  {
    [JsonProperty("attributes")]
    public JObject Attributes { get; set; }
  }

  public class Resource<T> : ResourceIdentifier<T>, IResource<T>
  {
    [JsonProperty("attributes")]
    public T Attributes { get; set; }
  }
}
