using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Models
{
    [Table("track_list")]
    public partial class TrackList
    {
        public TrackList()
        {
            PartyHistory = new HashSet<Party>();
            PartyQueue = new HashSet<Party>();
        }

        [Column("id")]
        public int Id { get; set; }
        [Required]
        [Column("data", TypeName = "json")]
        public string Data { get; set; }
        [Column("created_at", TypeName = "timestamp with time zone")]
        public DateTime CreatedAt { get; set; }
        [Column("updated_at", TypeName = "timestamp with time zone")]
        public DateTime UpdatedAt { get; set; }
        [Column("spotify_playlist_id")]
        public string SpotifyPlaylistId { get; set; }

        [InverseProperty("History")]
        public ICollection<Party> PartyHistory { get; set; }
        [InverseProperty("Queue")]
        public ICollection<Party> PartyQueue { get; set; }
    }
}
