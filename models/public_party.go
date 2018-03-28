package models

import (
	"encoding/json"

	"github.com/vattle/sqlboiler/types"
)

type PublicParty struct {
	Location     types.JSON    `json:"location"`
	RoomCode     string        `json:"room_code"`
	Ended        bool          `json:"ended"`
	Guests       *types.JSON   `json:"guests,omitempty"`
	CurrentTrack *PlayingTrack `json:"current_track,omitempty"`
}

func (p *Party) Public() (PublicParty, error) {
	response := PublicParty{
		Location: p.Location,
		RoomCode: p.RoomCode,
		Ended:    p.Ended,
	}

	guests, err := p.Guests()
	if err != nil {
		return PublicParty{}, err
	}

	if len(guests) > 0 {
		guestsPublic := make([]PublicGuest, 0)
		for _, guest := range guests {
			guestsPublic = append(guestsPublic, guest.Public())
		}
		guestsJsonRaw, _ := json.Marshal(guestsPublic)
		guestsJson := types.JSON(guestsJsonRaw)
		response.Guests = &guestsJson
	}

	if p.CurrentTrack.Valid {
		var track PlayingTrack
		err := p.CurrentTrack.Unmarshal(&track)
		if err == nil {
			response.CurrentTrack = &track
		}
	}

	return response, nil
}
