using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Server.Data
{
  public class ResourceIdentifier : IResourceIdentifier
  {
    [Required]
    [BindRequired]
    [JsonProperty("id")]
    public string Id { get; set; }

    [Required]
    [BindRequired]
    [JsonProperty("type")]
    public string Type { get; set; }

    [JsonProperty("meta", NullValueHandling = NullValueHandling.Ignore)]
    public JObject Metadata { get; set; }
  }

  public class ResourceIdentifier<T> : NewResourceIdentifier<T>, IResourceIdentifier
  {
    [Required]
    [BindRequired]
    [JsonProperty("id")]
    public string Id { get; set; }
  }
}
