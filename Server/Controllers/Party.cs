using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Server.Data;
using Server.Models;
using Server.Services;

namespace Server.Controllers
{
  public class Party : Controller
  {
    private RoomCodeGenerator _roomCodeGenerator;
    private UserProvider _userProvider;

    public Party(
      RoomCodeGenerator roomCodeGenerator,
      UserProvider userProvider
    )
    {
      _roomCodeGenerator = roomCodeGenerator;
      _userProvider = userProvider;
    }

    [HttpGet]
    [Route("/party")]
    public async Task<IActionResult> Index()
    {
      var user = await _userProvider.GetUserAsync();
      if (user == null) return Unauthorized();

      var party = user.Party;
      if (party == null) return NotFound();

      return Ok(Document.Resource(
        party.Id.ToString(),
        PublicParty.FromParty(party)
      ));
    }
  }
}
