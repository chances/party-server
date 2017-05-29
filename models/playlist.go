package models

import "github.com/zmb3/spotify"

type Playlist struct {
	ID          string `json:"id"`
	Name        string `json:"name"`
	Owner       string `json:"owner"`
	Endpoint    string `json:"endpoint"`
	TotalTracks uint   `json:"total_tracks"`
}

type CachedPlaylist struct {
	Playlist Playlist `json:"playlist"`
	Tracks   []Track
}

func NewPlaylist(p spotify.SimplePlaylist) Playlist {
	return Playlist{
		ID:          p.ID.String(),
		Name:        p.Name,
		Owner:       p.Owner.ID,
		Endpoint:    p.ExternalURLs["spotify"],
		TotalTracks: p.Tracks.Total,
	}
}

func NewPlaylistFromFullPlaylist(p *spotify.FullPlaylist) Playlist {
	return Playlist{
		ID:          p.ID.String(),
		Name:        p.Name,
		Owner:       p.Owner.ID,
		Endpoint:    p.ExternalURLs["spotify"],
		TotalTracks: uint(p.Tracks.Total),
	}
}

type Playlists struct {
	Playlists []Playlist
}
