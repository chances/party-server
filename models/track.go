package models

import (
	"time"

	"github.com/zmb3/spotify"
)

type Track struct {
	ID            string          `json:"id"`
	Name          string          `json:"name"`
	Artists       []TrackArtist   `json:"artists"`
	Images        []spotify.Image `json:"images"`
	BeganPlaying  time.Time       `json:"began_playing"`
	Duration      uint            `json:"duration"`
	Contributor   string          `json:"contributor"`
	ContributorID int             `json:"contributor_id"`
}

type TrackArtist struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

func NewTrack(t spotify.FullTrack) Track {
	track := Track{
		ID:            t.ID.String(),
		Name:          t.Name,
		Images:        t.Album.Images,
		Artists:       make([]TrackArtist, len(t.Artists)),
		BeganPlaying:  time.Unix(0, 0).UTC(),
		Duration:      uint(t.Duration / 1000),
		ContributorID: -1,
	}
	for i, artist := range t.Artists {
		track.Artists[i] = TrackArtist{
			ID:   artist.ID.String(),
			Name: artist.Name,
		}
	}
	return track
}

func NewTracks(t []spotify.PlaylistTrack) []Track {
	tracks := make([]Track, len(t))
	for i, track := range t {
		tracks[i] = NewTrack(track.Track)
	}
	return tracks
}

func (list *TrackList) Tracks() []Track {
	var tracks []Track
	list.Data.Unmarshal(&tracks)
	return tracks
}
