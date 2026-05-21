using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Newtonsoft.Json;
using Server.Data;

namespace Server.Models
{
  [ResourceIdentifier("join")]
  public class JoinParty
  {
    [Required]
    [BindRequired]
    [JsonProperty("room_code")]
    public string RoomCode { get; set; }
  }
}
