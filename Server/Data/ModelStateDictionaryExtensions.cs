using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.AspNetCore.Mvc.ModelBinding;

namespace Server.Data
{
  public static class ModelStateDictionaryExtensions
  {
    public static IEnumerable<string> Errors(this ModelStateDictionary modelState)
    {
      return modelState.Values
        .SelectMany(v => v.Errors.Select(b => {
          return string.IsNullOrWhiteSpace(b.ErrorMessage)
            ? (b.Exception?.Message ?? $"Unknown error binding '{v.AttemptedValue}'")
            : b.ErrorMessage;
        }));
    }
  }
}
