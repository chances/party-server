using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Models
{
    [Table("party")]
    public partial class Party
    {
        [Column("id")]
        public int Id { get; set; }
        [Required]
        [Column("location", TypeName = "json")]
        public string Location { get; set; }
        [Required]
        [Column("room_code", TypeName = "character varying(12)")]
        public string RoomCode { get; set; }
        [Column("ended")]
        public bool Ended { get; set; }
        [Column("current_track", TypeName = "json")]
        public string CurrentTrack { get; set; }
        [Column("queue_id")]
        public int QueueId { get; set; }
        [Column("history_id")]
        public int HistoryId { get; set; }
        [Column("guests_id")]
        public int GuestsId { get; set; }

        [ForeignKey("GuestsId")]
        [InverseProperty("Party")]
        public GuestList Guests { get; set; }
        [ForeignKey("HistoryId")]
        [InverseProperty("PartyHistory")]
        public TrackList History { get; set; }
        [ForeignKey("QueueId")]
        [InverseProperty("PartyQueue")]
        public TrackList Queue { get; set; }
        [InverseProperty("Party")]
        public User User { get; set; }
    }
}
