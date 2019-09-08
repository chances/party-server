using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Models;
using Newtonsoft.Json;

namespace Server.Models
{
  public static class PartyExtensions
  {
    private static readonly List<Track> EmptyTrackList = new List<Track>(0);

    public static PlayingTrack CurrentPlayingTrack(this Party party)
    {
      return party.CurrentTrack == null
        ? null
        : JsonConvert.DeserializeObject<PlayingTrack>(party.CurrentTrack);
    }

    public static void UpdateCurrentTrack(this Party party, PlayingTrack track)
    {
      party.CurrentTrack = JsonConvert.SerializeObject(track);
    }

    public static async Task<List<Track>> QueueTracks(this Party party, PartyModelContainer db)
    {
      await db.Entry(party).Reference(p => p.Queue).LoadAsync();
      var queue = party.Queue;

      if (queue == null || string.IsNullOrWhiteSpace(queue.Data))
      {
        return EmptyTrackList;
      }

      return JsonConvert.DeserializeObject<List<Track>>(queue.Data);
    }

    public static async Task UpdateQueue(this Party party, PartyModelContainer db, IList<Track> queueTracks)
    {
      await db.Entry(party).Reference(p => p.Queue).LoadAsync();
      var queue = party.Queue;

      if (queue == null)
      {
        throw new InvalidOperationException("All perties must have queues.");
      }

      queue.Data = JsonConvert.SerializeObject(queueTracks);
      queue.UpdatedAt = DateTime.UtcNow;
    }

    public static async Task<List<Track>> HistoryTracks(this Party party, PartyModelContainer db)
    {
      await db.Entry(party).Reference(p => p.History).LoadAsync();
      var history = party.History;

      if (history == null || string.IsNullOrWhiteSpace(history.Data))
      {
        return EmptyTrackList;
      }

      return JsonConvert.DeserializeObject<List<Track>>(history.Data);
    }

    public static async Task UpdateHistory(this Party party, PartyModelContainer db, IList<Track> historyTracks)
    {
      await db.Entry(party).Reference(p => p.History).LoadAsync();
      var history = party.History;

      if (history == null)
      {
        throw new InvalidOperationException("All perties must have track histories.");
      }

      history.Data = JsonConvert.SerializeObject(historyTracks);
      history.UpdatedAt = DateTime.UtcNow;
    }

    public static List<Guest> GuestList(this Party party)
    {
      var guests = party.Guests;

      if (guests == null || string.IsNullOrWhiteSpace(guests.Data))
      {
        return new List<Guest>(0);
      }

      return JsonConvert.DeserializeObject<List<Guest>>(guests.Data);
    }

    public static async Task<List<Guest>> GuestList(this Party party, PartyModelContainer db)
    {
      await db.Entry(party).Reference(p => p.Guests).LoadAsync();

      return party.GuestList();
    }

    public static void UpdateGuestList(this Party party, List<Guest> guests)
    {
      if (party.Guests == null)
      {
        party.Guests = new GuestList();
      }

      party.Guests.Data = JsonConvert.SerializeObject(guests);
    }
  }
}
