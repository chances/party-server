using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Server.Data;

namespace Server.Models
{
  [ResourceIdentifier("pause")]
  public class PauseParameters
  {

    [Required]
    [BindRequired]
    public uint Elapsed { get; set; }
  }
}
