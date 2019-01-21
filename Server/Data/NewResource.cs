using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Server.Data
{
  public class NewResource<T> : IValidatableObject
  {
    [Required]
    [BindRequired]
    [JsonProperty("type")]
    public string Type { get; set; }

    [JsonIgnore]
    public string ExpectedType => typeof(T).Name.ToSnakeCase();

    [JsonProperty("attributes")]
    public T Attributes { get; set; }

    [JsonProperty("meta", NullValueHandling = NullValueHandling.Ignore)]
    public JObject Metadata { get; set; }

    public IEnumerable<ValidationResult> Validate(ValidationContext validationContext)
    {
      if (Type != ExpectedType)
      {
        yield return new ValidationResult(
          $"This Resource Identifier's type must match '{ExpectedType}'."
        );
      }
    }
  }
}
