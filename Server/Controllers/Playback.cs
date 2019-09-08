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

    /// <summary>
    /// Play the party's playlist or resume from paused state
    /// </summary>
    /// <param name="parameters"></param>
    /// <returns></returns>
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
        return Ok(ResourceDocument<PlayingTrack>.Resource(currentTrack.Id, currentTrack));
      }

      await _db.SaveChangesAsync();

      // TODO: Send party update event to Party hub
      // TODO: Send queue update event to Party hub

      return Ok(ResourceDocument<PlayingTrack>.Resource(currentTrack.Id, currentTrack));
    }

    /// <summary>
    /// Pause the party's playlist from playing
    /// </summary>
    /// <returns></returns>
    [HttpPost]
    [Route("/pause")]
    public async Task<IActionResult> Pause([FromBody] NewResourceDocument<PauseParameters> parameters)
    {
      var currentParty = await _partyProvider.GetCurrentPartyAsync(_db);
      if (currentParty == null) return Error.NotFound("Host has not started a party");

      var currentTrack = currentParty.CurrentPlayingTrack();

      // It's a bad request to pause when no track is playing
      if (currentTrack == null)
      {
        return Error.BadRequest("Host is not playing music");
      }

      // Don't update elapsed if the current track is already paused
      if (currentTrack.Paused)
      {
        return Ok(ResourceDocument<PlayingTrack>.Resource(currentTrack.Id, currentTrack));
      }

      // Pause the current track
      currentTrack.Paused = true;
      currentTrack.Elapsed = parameters.Data.Attributes.Elapsed;

      currentParty.UpdateCurrentTrack(currentTrack);
      await _db.SaveChangesAsync();

      // TODO: Send party update event to Party hub

      return Ok(ResourceDocument<PlayingTrack>.Resource(currentTrack.Id, currentTrack));
    }

    [HttpPost]
    [Route("/skip")]
    public async Task<IActionResult> Skip()
    {
      var currentParty = await _partyProvider.GetCurrentPartyAsync(_db);
      if (currentParty == null) return Error.NotFound("Host has not started a party");

      var lastTrack = currentParty.CurrentPlayingTrack();

      // It's a bad request to skip when no track is playing
      if (lastTrack == null)
      {
        return Error.BadRequest("Host is not playing music");
      }

      // Pop next track from the party's queue
      var queue = await currentParty.QueueTracks(_db);
      var history = await currentParty.HistoryTracks(_db);
      var newTrack = await PopTrackAndPlay(queue, currentParty.QueueId, currentParty);

      // Push last track to party's history
      history.Insert(0, lastTrack);
      await currentParty.UpdateHistory(_db, history);

      await _db.SaveChangesAsync();

      // TODO: Send queue update event to Party hub
      // TODO: Send history update event to Party hub

      return Ok(ResourceDocument<PlayingTrack>.Resource(newTrack.Id, newTrack));
    }

    private async Task<PlayingTrack> PopTrackAndPlay(IList<Track> tracks, int queueId, Db.Party party)
    {
      var firstTrack = tracks.FirstOrDefault();
      var updatedQueue = tracks.Skip(1).ToList();

      await party.UpdateQueue(_db, updatedQueue);

      var playingTrack = (PlayingTrack) firstTrack;
      Debug.Assert(playingTrack != null, nameof(playingTrack) + " != null");
      // "Play" the track
      playingTrack.Paused = false;
      playingTrack.Elapsed = 0;
      playingTrack.BeganPlaying = DateTime.UtcNow;

      party.UpdateCurrentTrack(playingTrack);

      return playingTrack;
    }
  }
}
