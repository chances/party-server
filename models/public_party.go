package models

import "github.com/vattle/sqlboiler/types"

type PublicParty struct {
  Location     types.JSON    `json:"location"`
  RoomCode     string        `json:"room_code"`
  Ended        bool          `json:"ended"`
  Guests       *types.JSON   `json:"guests,omitempty"`
  CurrentTrack *Track `json:"current_track,omitempty"`
}
