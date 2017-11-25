package models

import (
	"log"
	"math/rand"
	"time"

	"github.com/zmb3/spotify"
)

type Playlist struct {
	ID          string `json:"id"`
	Name        string `json:"name"`
	Owner       string `json:"owner"`
	Endpoint    string `json:"endpoint"`
	TotalTracks uint   `json:"total_tracks"`
}

type CachedPlaylist struct {
	Playlist Playlist `json:"playlist"`
	Tracks   []Track  `json:"tracks"`
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

// QueueTracks gets this party's queue track list
func (p *Party) QueueTracks() (*[]Track, error) {
	queueData, err := p.QueueG().One()
	if err != nil {
		return nil, err
	}

	var queue []Track
	err = queueData.Data.Unmarshal(&queue)
	if err != nil {
		return nil, err
	}

	return &queue, nil
}

// HistoryTracks gets this party's history track list
func (p *Party) HistoryTracks() (*[]Track, error) {
	historyData, err := p.HistoryG().One()
	if err != nil {
		return nil, err
	}

	var history []Track
	err = historyData.Data.Unmarshal(&history)
	if err != nil {
		return nil, err
	}

	return &history, nil
}

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

// Shuffle a playlist distributing the same artist "evenly" throughout
func Shuffle(playlist *[]Track) *[]Track {
	shuffled := make([]Track, len(*playlist))
	artists, longestArtistListLength := artists(*playlist)

	log.Printf("There are %d artists\n", len(artists))
	log.Printf("Longest artist list length: %d", longestArtistListLength)

	// Shuffle: https://labs.spotify.com/2014/02/28/how-to-shuffle-songs/
	// http://keyj.emphy.de/balanced-shuffle/

	if longestArtistListLength > 1 {
		// TODO: Fisher-Yates shuffle the tracks in the artist arrays
		// TODO: Spread songs in artist arrays of length, longestArtistListLength, for each artist
		// Spotify's algorithm takes into account how long a playlist is, and how many of each type
		//  of song there are. So if there are four White Stripes songs in the list, they will each
		//   appear at roughly 25% intervals.
		return playlist
	} else {
		// Fisher-Yates shuffle the artist array
		N := len(artists)
		for i := 0; i < N; i++ {
			// choose index uniformly in [i, N-1]
			r := i + rand.Intn(N-i)
			artists[r], artists[i] = artists[i], artists[r]
		}

		for i, tracks := range artists {
			shuffled[i] = *tracks[0]
		}
	}

	return &shuffled
}

func artists(tracks []Track) ([]([]*Track), int) {
	// Split up tracks by artist, accumulating a longest list length
	artists := make(map[string]([]*Track))
	longestLength := 0
	for _, track := range tracks {
		artist := track.FirstArtist()
		_, exists := artists[artist]
		if exists {
			artists[artist] = append(artists[artist], &track)
			length := len(artists[artist])
			if length > longestLength {
				longestLength = length
			}
		}
		artists[artist] = []*Track{&track}
		if longestLength == 0 {
			longestLength++
		}
	}
	// Convert map to array of arrays of tracks by artist
	artistsArray := make([]([]*Track), len(artists))
	i := 0
	for keys := range artists {
		artistsArray[i] = artists[keys]
		i++
	}
	return artistsArray, longestLength
}

// FirstArtist gets a track's first artist, or "Unknown" if no artists exist
func (t *Track) FirstArtist() string {
	if len(t.Artists) == 0 {
		return "Unknown"
	}

	return t.Artists[0].Name
}
