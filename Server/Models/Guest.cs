using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using Models;
using Newtonsoft.Json;
using Npgsql;

namespace Server.Models
{
  public class Guest
  {
    [JsonProperty("name")]
    public string Name { get; set; }

    [JsonProperty("alias")]
    public string Alias { get; set; }

    [JsonProperty("checked_in")]
    public bool CheckedIn { get; set; }

    [JsonProperty("token")]
    public string Token { get; set; }

    public static async Task<Guest> GetByToken(DbSet<GuestList> db, string token)
    {
      var sql = "SELECT guests.id, guests.data FROM " +
        "(SELECT id, data, json_array_elements(data) AS guest FROM guest_list)" +
        " AS guests WHERE guest->>'token'=@token";
      var tokenParam = new NpgsqlParameter("token", token);

      var guestList = await db.FromSql(sql, tokenParam).FirstOrDefaultAsync();
      if (guestList == null || string.IsNullOrWhiteSpace(guestList.Data))
      {
        return null;
      }

      return JsonConvert.DeserializeObject<List<Guest>>(guestList.Data)
        .FirstOrDefault(g => g.Token == token);
    }
  }
}
