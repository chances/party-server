using System;
using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;
using Spotify.API.NetCore.Models;

namespace Server.Models
{
  public class Track
  {
    public Track()
    {
      Artists = new List<TrackArtist>();
      Images = new List<Image>();
    }

    [JsonProperty("id")]
    public string Id { get; set; }

    [JsonProperty("name")]
    public string Name { get; set; }

    [JsonProperty("artists")]
    public List<TrackArtist> Artists { get; set; }

    [JsonIgnore]
    public string FirstArtist => Artists
      .Select(a => a.Name)
      .DefaultIfEmpty("Unknown")
      .FirstOrDefault();

    [JsonProperty("images")]
    public List<Image> Images { get; set; }

    [JsonProperty("endpoint")]
    public string Endpoint { get; set; }

    [JsonProperty("began_playing")]
    public DateTime? BeganPlaying { get; set; }

    [JsonProperty("duration")]
    public int Duration { get; set; }

    [JsonProperty("contributor")]
    public string Contributor { get; set; }

    [JsonProperty("contributor_id")]
    public int ContributorID { get; set; }

    public static Track FromSpotify(FullTrack track) => new Track()
    {
      Id = track.Id,
      Name = track.Name,
      Artists = track.Artists.Select(a => new TrackArtist() { Id = a.Id, Name = a.Name}).ToList(),
      Images = track.Album.Images,
      Endpoint = track.Href,
      BeganPlaying = null,
      Duration = (int) Math.Round(track.DurationMs / 1000.0),
      Contributor = null,
      ContributorID = -1
    };
  }
}
