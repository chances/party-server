using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Db = Models;
using Server.Data;
using Server.Models;
using Server.Services;

namespace Server.Controllers
{
  [Route("/party")]
  public class Playback : Controller
  {
    private readonly PartyProvider _partyProvider;
    private readonly Db.PartyModelContainer _db;

    public Playback(PartyProvider partyProvider, Db.PartyModelContainer db)
    {
      _partyProvider = partyProvider;
      _db = db;
    }

    [HttpPost]
    [Route("/play")]
    public async Task<IActionResult> Play([FromBody] NewResourceDocument<PlayParameters> parameters = null)
    {
      var playParameters = parameters?.Data.Attributes ?? PlayParameters.Default;

      var currentParty = await _partyProvider.GetCurrentPartyAsync(_db);
      if (currentParty == null) return Error.NotFound("Host has not started a party");

      var currentTrack = currentParty.CurrentPlayingTrack();

      if (currentTrack == null)
      {
        // Begin playing the party's queue
        var queue = await currentParty.QueueTracks(_db);

        // Shuffle the queue if requested
        if (playParameters.Shuffle)
        {
          Playlist.Shuffle(queue);
        }

        currentTrack = await PopTrackAndPlay(queue, currentParty.QueueId, currentParty);
      }
      else if (currentTrack.Paused)
      {
        // Resume the current track
        currentTrack.Paused = false;
        currentTrack.BeganPlaying = DateTime.UtcNow.AddSeconds(
          -1.0 * currentTrack.Elapsed
        );
      }
      else
      {
        return Ok();
      }

      currentParty.UpdateCurrentTrack(currentTrack);
      await _db.SaveChangesAsync();

      // TODO: Send party update event to Party hub

      return Ok();
    }

    [HttpPost]
    [Route("/pause")]
    public async Task<IActionResult> Pause()
    {
      return Ok();
    }

    [HttpPost]
    [Route("/skip")]
    public async Task<IActionResult> Skip()
    {
      return Ok();
    }

    private async Task<PlayingTrack> PopTrackAndPlay(IList<Track> tracks, int queueId, Db.Party party)
    {
      var firstTrack = tracks.FirstOrDefault();
      var updatedQueue = tracks.Skip(1).ToList();

      await party.UpdateQueue(_db, updatedQueue);

      // TODO: Update Party's queue TrackList
      // TODO: Send queue update event to Party hub

      var playingTrack = (PlayingTrack) firstTrack;
      Debug.Assert(playingTrack != null, nameof(playingTrack) + " != null");
      // "Play" the track
      playingTrack.Paused = false;
      playingTrack.Elapsed = 0;
      playingTrack.BeganPlaying = DateTime.UtcNow;

      return playingTrack;
    }
  }
}
