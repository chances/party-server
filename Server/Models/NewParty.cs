using System;
using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;

namespace Server.Models
{
  public class NewParty
  {
    [Required]
    [BindRequired]
    [JsonProperty("location")]
    public Location Location { get; set; }

    [Required]
    [BindRequired]
    [JsonProperty("playlist_id")]
    public string PlaylistId { get; set; }
  }
}
