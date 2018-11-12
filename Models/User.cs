using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Models
{
    [Table("user")]
    public partial class User
    {
        [Column("id")]
        public long Id { get; set; }
        [Required]
        [Column("username", TypeName = "character varying")]
        public string Username { get; set; }
        [Required]
        [Column("spotify_user", TypeName = "json")]
        public string SpotifyUser { get; set; }
        [Column("spotify_playlist_id", TypeName = "character varying")]
        public string SpotifyPlaylistId { get; set; }
        [Required]
        [Column("access_token", TypeName = "character varying")]
        public string AccessToken { get; set; }
        [Required]
        [Column("refresh_token", TypeName = "character varying")]
        public string RefreshToken { get; set; }
        [Column("token_expiry_date", TypeName = "timestamp with time zone")]
        public DateTime TokenExpiryDate { get; set; }
        [Required]
        [Column("token_scope", TypeName = "character varying")]
        public string TokenScope { get; set; }
        [Column("created_at", TypeName = "timestamp with time zone")]
        public DateTime CreatedAt { get; set; }
        [Column("updated_at", TypeName = "timestamp with time zone")]
        public DateTime UpdatedAt { get; set; }
        [Column("party_id")]
        public int? PartyId { get; set; }

        [ForeignKey("PartyId")]
        [InverseProperty("User")]
        public Party Party { get; set; }
    }
}
