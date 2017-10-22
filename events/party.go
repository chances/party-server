package events

import (
	"encoding/json"

	"github.com/chances/party-server/models"
)

// UpdateParty broadcasts a party update to subscribed clients
func UpdateParty(party models.PublicParty) {
	partyJson, _ := json.Marshal(party)
	Event(party.RoomCode + "party").Submit(string(partyJson))
}
