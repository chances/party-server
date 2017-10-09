package spotify

import (
	"github.com/chances/party-server/cache"
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
		if err != nil {
			return nil, err
		}

		var playlistEntry cache.Entry
		if trackList != nil {
			var playlist models.CachedPlaylist
			err := trackList.Data.Unmarshal(&playlistEntry)
			if err != nil {
				return nil, err
			}
			playlistEntry = cache.Forever(playlist)
		} else {
			playlist, err := client.GetPlaylist(username, spotify.ID(playlistID))
			// TODO: Page through all tracks
			if err != nil {
				return nil, err
			}

			tracks := models.NewTracks(playlist.Tracks.Tracks)
			playlistEntry = cache.Forever(
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
	return *playlistEntry, nil
}

// Playlists gets the current user's playlists
func Playlists(username string, client spotify.Client) ([]models.Playlist, error) {
	playlistsEntry, err := partyCache.GetOrDefer("playlists:"+username, func() (*cache.Entry, error) {
		playlists, err := playlists(client)
		if err != nil {
			return nil, err
		}
		playlistsEntry := cache.Forever(playlists)
		return &playlistsEntry, nil
	})
	if err != nil {
		return nil, err
	}

	playlists := (*playlistsEntry).Value.([]models.Playlist)
	return playlists, nil
}

func playlists(client spotify.Client) ([]models.Playlist, error) {
	limit := 50
	playlistPage, err := client.CurrentUsersPlaylistsOpt(&spotify.Options{
		Limit: &limit,
	})
	if err != nil {
		// TODO: Fix "The access token expired" errors
		return nil, err
	}

	playlists := make([]models.Playlist, len(playlistPage.Playlists))

	for i, playlist := range playlistPage.Playlists {
		go cachePlaylist(client, playlist)
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
