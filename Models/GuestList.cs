using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Models
{
    [Table("guest_list")]
    public partial class GuestList
    {
        public GuestList()
        {
            Party = new HashSet<Party>();
        }

        [Column("id")]
        public int Id { get; set; }
        [Required]
        [Column("data", TypeName = "json")]
        public string Data { get; set; }

        [InverseProperty("Guests")]
        public ICollection<Party> Party { get; set; }
    }
}
