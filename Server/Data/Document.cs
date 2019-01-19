using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace Server.Data
{
  public class Document
  {
    [JsonProperty("meta", NullValueHandling = NullValueHandling.Ignore)]
    public JObject Metadata { get; set; }

    public static Document<T> Resource<T>(string id, T data) => new Document<T>(
      new Resource<T> { Id = id, Attributes = data }
    );

    public static Document Error(Error error) => new ErrorDocument(error);

    public static Document Errors(IEnumerable<Error> errors) =>
      new ErrorDocument(errors.ToList());
  }

  public class Document<T> : Document
  {
    [Required]
    [BindRequired]
    [JsonProperty("data", NullValueHandling = NullValueHandling.Ignore)]
    public Resource<T> Data { get; set; }

    public Document(Resource<T> data = null)
    {
      Data = data;
    }
  }

  public class ErrorDocument : Document
  {
    [Required]
    [BindRequired]
    [JsonProperty("errors", NullValueHandling = NullValueHandling.Ignore)]
    public new List<Error> Errors { get; }

    public ErrorDocument(Error error) : this(new List<Error> { error })
    {
    }

    public ErrorDocument(List<Error> errors = null)
    {
      Errors = errors;
    }
  }

  public class ResourceIdentifierDocument<T> : Document
  {
    [Required]
    [BindRequired]
    [JsonProperty("data", NullValueHandling = NullValueHandling.Ignore)]
    public ResourceIdentifier<T> Data { get; set; }
  }
}
