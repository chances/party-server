using System;
using System.Collections.Generic;
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

  public class ResourceIdentifier<T> : IResourceIdentifier, IValidatableObject
  {
    [Required]
    [BindRequired]
    [JsonProperty("id")]
    public string Id { get; set; }

    [Required]
    [BindRequired]
    [JsonProperty("type")]
    public string Type { get; set; }

    [JsonIgnore]
    public string ExpectedType => typeof(T).Name.ToSnakeCase();

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
