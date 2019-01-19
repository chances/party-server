using System;
using System.Collections.Generic;
using System.Linq;
using Newtonsoft.Json;
using Spotify.API.NetCore.Models;

namespace Server.Models
{
  public class Playlist
  {
    [JsonProperty("id")]
    public string Id { get; set; }

    [JsonProperty("id")]
    public string Name { get; set; }

    [JsonProperty("owner")]
    public string Owner { get; set; }

    [JsonProperty("endpoint")]
    public string Endpoint { get; set; }

    [JsonProperty("total_tracks")]
    public int TotalTracks { get; set; }

    public static Playlist FromSpotify(SimplePlaylist playlist) => new Playlist()
    {
      Id = playlist.Id,
      Name = playlist.Name,
      Owner = playlist.Owner.Id,
      Endpoint = playlist.Href,
      TotalTracks = playlist.Tracks.Total
    };

    public static Playlist FromSpotify(FullPlaylist playlist) => new Playlist()
    {
      Id = playlist.Id,
      Name = playlist.Name,
      Owner = playlist.Owner.Id,
      Endpoint = playlist.Href,
      TotalTracks = playlist.Tracks.Total
    };

    /// <summary>
    /// Shuffle the specified playlist, distributing the same artist "evenly" throughout
    /// </summary>
    /// <returns>The shuffled playlist.</returns>
    /// <param name="playlist">Playlist to shuffle.</param>
    public static IList<Track> Shuffle(IList<Track> playlist)
    {
      var tracksByArtist = playlist
        .GroupBy(t => t.FirstArtist)
        .ToDictionary(g => g.Key, g => g.ToList());
      var longestArtistListLength = tracksByArtist.Values
        .Select(l => l.Count).Max();

      var seed = (int) DateTime.UtcNow.Subtract(new DateTime(1970, 1, 1)).TotalSeconds;
      var random = new Random(seed);

      // Shuffle: https://labs.spotify.com/2014/02/28/how-to-shuffle-songs/
      // http://keyj.emphy.de/balanced-shuffle/

      List<Track> shuffled;

      if (longestArtistListLength > 1)
      {
        shuffled = new List<Track>(playlist.Count);

        // TODO: Spread songs apart from those with the same artist
        //
        // Spotify's algorithm takes into account how long a playlist is, and how many of each type
        //  of song there are. So if there are four White Stripes songs in the list, they will each
        //   appear at roughly 25% intervals.
        //
        // This algorithm simply picks random song artists, but won't pick the same artist twice
        string lastArtist = null;
        int i;
        while (tracksByArtist.Keys.Count > 0)
        {
          // Select a random artist that does not equal the last artist
          do
          {
            i = random.Next(tracksByArtist.Keys.Count);
          } while (tracksByArtist.Keys.ElementAt(i) == lastArtist);

          // Pop a random track from the chosen artist's track list
          var tracks = tracksByArtist.Values.ElementAt(i);
          var v = random.Next(tracks.Count);
          var track = tracks[v];
          tracks.RemoveAt(v);

          // Append the chosen track
          shuffled[shuffled.Count == 0 ? 0 : shuffled.Count - 1] = track;
        }

        return shuffled;
      }

      // Fisher-Yates shuffle the playlist
      shuffled = playlist.ToList();
      var N = shuffled.Count;
      for (int i = 0; i < N; i += 1)
      {
        var r = i + random.Next(N - i);
        shuffled.Swap(i, r);
      }

      return shuffled;
    }
  }
}
