using System;
using System.Collections.Generic;
using Models;
using Newtonsoft.Json;

namespace Server.Models
{
  public static class PartyExtensions
  {
    private static readonly List<Track> EmptyTrackList = new List<Track>(0);

    public static List<Track> QueueTracks(this Party party)
    {
      var queue = party.Queue;

      if (queue == null || string.IsNullOrWhiteSpace(queue.Data))
      {
        return EmptyTrackList;
      }

      return JsonConvert.DeserializeObject<List<Track>>(queue.Data);
    }

    public static List<Track> HistoryTracks(this Party party)
    {
      var history = party.History;

      if (history == null || string.IsNullOrWhiteSpace(history.Data))
      {
        return EmptyTrackList;
      }

      return JsonConvert.DeserializeObject<List<Track>>(history.Data);
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
