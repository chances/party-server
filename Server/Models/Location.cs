using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;

namespace Server.Models
{
  public class Location
  {
    [Required]
    [BindRequired]
    [JsonProperty("host_name")]
    public string HostName { get; set; }
  }
}
