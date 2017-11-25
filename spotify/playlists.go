package spotify

import (
	"database/sql"
	"time"

	"github.com/chances/party-server/cache"
	e "github.com/chances/party-server/errors"
	"github.com/chances/party-server/models"
	"github.com/vattle/sqlboiler/boil"
	"github.com/vattle/sqlboiler/queries/qm"
	"github.com/zmb3/spotify"
	"gopkg.in/nullbio/null.v6"
)

// Playlist gets a Spotify user's playlist given its username and playlist ID
func Playlist(username string, playlistID string, client spotify.Client) (models.CachedPlaylist, error) {
	playlistEntry, err := playlist(username, playlistID, client)
	if err != nil {
		return models.CachedPlaylist{}, err
	}
	cachedPlaylist := playlistEntry.Value.(models.CachedPlaylist)
	return cachedPlaylist, nil
}

func playlist(username string, playlistID string, client spotify.Client) (cache.Entry, error) {
	// Try to get CachedPlaylist, if not in cache try from DB, else get from Spotify API
	playlistEntry, err := partyCache.GetOrDefer("playlist:"+playlistID, func() (*cache.Entry, error) {
		trackList, err := models.TrackListsG(qm.Where("spotify_playlist_id=?", playlistID)).One()
		if err != nil && err != sql.ErrNoRows {
			return nil, err
		}

		// Only cache playlists in the DB for one day
		now := time.Now().UTC()
		oneHour := time.Duration(1) * time.Hour
		oneDay := time.Duration(24) * time.Hour
		dbCacheValidWindow := now.Add(oneDay * -1)
		dbCacheEntryValid := err != sql.ErrNoRows && trackList.UpdatedAt.After(dbCacheValidWindow)

		var playlistEntry cache.Entry
		if trackList != nil && dbCacheEntryValid {
			var playlist models.CachedPlaylist
			err := trackList.Data.Unmarshal(&playlistEntry)
			if err != nil {
				return nil, err
			}
			playlistEntry = cache.ExpiresRolling(time.Now().UTC().Add(oneHour), playlist)
		} else {
			playlist, err := client.GetPlaylist(username, spotify.ID(playlistID))
			if err != nil {
				return nil, err
			}

			tracksPage := playlist.Tracks
			tracks := models.NewTracks(tracksPage.Tracks)

			// Page through all of the playlist's tracks
			limit := 50
			for offset := tracksPage.Offset + tracksPage.Limit; offset < tracksPage.Total; offset += tracksPage.Limit {
				page, err := client.GetPlaylistTracksOpt(username, spotify.ID(playlistID), &spotify.Options{
					Offset: &offset,
					Limit:  &limit,
				}, "")
				if err != nil {
					return nil, err
				}

				pageTracks := models.NewTracks(page.Tracks)
				tracks = append(tracks, pageTracks...)

				tracksPage = *page
			}

			// Cache playlists in a one hour window, rolling forward when accessed
			playlistEntry = cache.ExpiresRolling(
				time.Now().UTC().Add(oneHour),
				models.CachedPlaylist{
					Playlist: models.NewPlaylistFromFullPlaylist(playlist),
					Tracks:   tracks,
				},
			)
			go func() {
				var trackList models.TrackList
				trackList.SpotifyPlaylistID = null.StringFrom(playlist.ID.String())
				trackList.Data.Marshal(&playlistEntry.Value)
				trackList.Upsert(boil.GetDB(), true, []string{"spotify_playlist_id"}, []string{"data"})
			}()
		}

		return &playlistEntry, nil
	})
	if err != nil {
		return cache.Entry{}, err
	}
	if playlistEntry == nil {
		return cache.Entry{}, e.Internal.WithDetail("Playlist cache entry is nil")
	}
	return *playlistEntry, nil
}

// Playlists gets the current user's playlists
func Playlists(username string, client spotify.Client) ([]models.Playlist, error) {
	playlists, err := playlists(client)
	if err != nil {
		return nil, err
	}

	return playlists, nil
}

func playlists(client spotify.Client) ([]models.Playlist, error) {
	limit := 50
	// TODO: Only get the necessary fields for the list fo playlists
	playlistPage, err := client.CurrentUsersPlaylistsOpt(&spotify.Options{
		Limit: &limit,
	})
	// TODO: Page through all of the user's playlists
	if err != nil {
		// TODO: Fix "The access token expired" errors
		return nil, err
	}

	playlists := make([]models.Playlist, len(playlistPage.Playlists))

	for i, playlist := range playlistPage.Playlists {
		// Don't cache *all* the playlists so aggressively.
		// go cachePlaylist(client, playlist)
		playlists[i] = models.NewPlaylist(playlist)
	}

	return playlists, nil
}

func cachePlaylist(client spotify.Client, playlist spotify.SimplePlaylist) error {
	tracksPage, err := client.GetPlaylistTracks(playlist.Owner.ID, playlist.ID)
	// TODO: Page through all tracks
	if err != nil {
		return err
	}

	tracks := models.NewTracks(tracksPage.Tracks)
	playlistEntry := cache.Forever(
		models.CachedPlaylist{
			Playlist: models.NewPlaylist(playlist),
			Tracks:   tracks,
		},
	)
	go func() {
		var trackList models.TrackList
		trackList.SpotifyPlaylistID = null.StringFrom(playlist.ID.String())
		trackList.Data.Marshal(&playlistEntry.Value)
		trackList.Upsert(boil.GetDB(), true, []string{"spotify_playlist_id"}, []string{"data"})
	}()
	partyCache.Set("playlist:"+playlist.ID.String(), playlistEntry)
	return nil
}
