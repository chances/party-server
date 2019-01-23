﻿using System;
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
    public ResourceIdentifier()
    {
      Type = ExpectedType;
    }

    [Required]
    [BindRequired]
    [JsonProperty("id")]
    public string Id { get; set; }

    [Required]
    [BindRequired]
    [JsonProperty("type")]
    public string Type { get; set; }

    [JsonIgnore]
    private static string ExpectedType
    {
      get
      {
        var type = typeof(T);
        var resourceIdAttribute =
          (ResourceIdentifierAttribute) Attribute.GetCustomAttribute(type, typeof(ResourceIdentifierAttribute));

        return resourceIdAttribute != null
          ? resourceIdAttribute.Type
          : type.Name.ToKebabCase();
      }
    }

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
