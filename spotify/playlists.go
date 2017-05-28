package spotify

import (
	"github.com/chances/chances-party/cache"
	"github.com/chances/chances-party/models"
	"github.com/vattle/sqlboiler/boil"
	"github.com/zmb3/spotify"
	"gopkg.in/nullbio/null.v6"
)

// Playlists gets the current user's playlists
func Playlists(partyCache cache.Store, username string, client spotify.Client) (*[]models.Playlist, error) {
	playlistsEntry, err := partyCache.GetOrDefer("playlists:"+username, func() (*cache.Entry, error) {
		playlists, err := _playlists(partyCache, client)
		if err != nil {
			return nil, err
		}
		playlistsEntry := cache.Forever(playlists)
		return &playlistsEntry, nil
	})
	if err != nil {
		return nil, err
	}

	playlists := (*playlistsEntry.Value).(*[]models.Playlist)
	return playlists, nil
}

func _playlists(partyCache cache.Store, client spotify.Client) (*[]models.Playlist, error) {
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
		go func() {
			playlistEntry, _ := cachePlaylist(client, playlist)
			if playlistEntry == nil {
				return
			}
			partyCache.Set("playlist:"+playlist.ID.String(), *playlistEntry)
		}()
		go cachePlaylist(client, playlist)
		playlists[i] = models.NewPlaylist(playlist)
	}

	return &playlists, nil
}

func cachePlaylist(client spotify.Client, playlist spotify.SimplePlaylist) (*cache.Entry, error) {
	tracksPage, err := client.GetPlaylistTracks(playlist.Owner.ID, playlist.ID)
	// TODO: Page through all tracks
	if err != nil {
		return nil, err
	}

	tracks := models.NewTracks(tracksPage.Tracks)
	go func() {
		var trackList models.TrackList
		trackList.SpotifyPlaylistID = null.StringFrom(playlist.ID.String())
		trackList.Data.Marshal(&tracks)
		trackList.Upsert(boil.GetDB(), true, []string{"spotify_playlist_id"}, []string{"data"})
	}()
	playlistEntry := cache.Forever(
		models.CachedPlaylist{
			Playlist: models.NewPlaylist(playlist),
			Tracks:   tracks,
		},
	)
	return &playlistEntry, nil
}
