using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;

namespace Server.Data
{
  public class NewResource<T> : NewResourceIdentifier<T>
  {
    [Required]
    [BindRequired]
    [JsonProperty("attributes")]
    public T Attributes { get; set; }
  }
}
