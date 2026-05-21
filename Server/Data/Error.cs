using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using Microsoft.AspNetCore.Mvc;
using Newtonsoft.Json;

namespace Server.Data
{
  public class Error
  {
    [JsonProperty("id", NullValueHandling = NullValueHandling.Ignore)]
    string Id { get; set; }

    [JsonProperty("status", NullValueHandling = NullValueHandling.Ignore)]
    string Status { get; set; }

    [JsonProperty("code", NullValueHandling = NullValueHandling.Ignore)]
    string Code { get; set; }

    [JsonProperty("title", NullValueHandling = NullValueHandling.Ignore)]
    string Title { get; set; }

    [JsonProperty("detail", NullValueHandling = NullValueHandling.Ignore)]
    string Detail { get; set; }

    [JsonProperty("meta", NullValueHandling = NullValueHandling.Ignore)]
    object Metadata { get; set; }

    public Error(int? status = null, int? code = null)
    {
      Status = status?.ToString();
      Code = code?.ToString();
    }

    public static BadRequestObjectResult BadRequest(IEnumerable<string> errorDetails)
    {
      var errors = new List<Error>(
        errorDetails.Select(detail => new Error() { Detail = detail })
      );

      var badRequestError = new Error((int)HttpStatusCode.BadRequest)
      {
        Id = Enum.GetName(typeof(HttpStatusCode), HttpStatusCode.BadRequest).ToSnakeCase(),
      };
      errors.Insert(0, badRequestError);

      return new BadRequestObjectResult(Document.Errors(errors));
    }

    public static BadRequestObjectResult BadRequest(string detail = null) => new BadRequestObjectResult(
      Document.Error(new Error((int) HttpStatusCode.BadRequest)
      {
        Id = Enum.GetName(typeof(HttpStatusCode), HttpStatusCode.BadRequest).ToSnakeCase(),
        Detail = detail
      })
    );

    public static NotFoundObjectResult NotFound(string detail = null) => new NotFoundObjectResult(
      Document.Error(new Error((int)HttpStatusCode.NotFound)
      {
        Id = Enum.GetName(typeof(HttpStatusCode), HttpStatusCode.NotFound).ToSnakeCase(),
        Detail = detail
      })
    );

    public static NotFoundObjectResult Internal(string detail = null) => new NotFoundObjectResult(
      Document.Error(new Error((int)HttpStatusCode.InternalServerError)
      {
        Id = Enum.GetName(typeof(HttpStatusCode), HttpStatusCode.InternalServerError).ToSnakeCase(),
        Detail = detail
      })
    );
  }
}
