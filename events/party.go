package events

import (
	"encoding/json"

	"github.com/chances/party-server/models"
)

// UpdateParty broadcasts a party update to subscribed clients
func UpdateParty(party models.PublicParty) {
	partyJSON, _ := json.Marshal(party)
	Event(party.RoomCode + "party").Submit(string(partyJSON))
}

// UpdateHistory broadcasts a history track list update to subscribed clients
func UpdateHistory(history *[]models.Track, partyCode string) {
	historyJSON, _ := json.Marshal(history)
	Event(partyCode + "history").Submit(string(historyJSON))
}

// UpdateQueue broadcasts a queue track list update to subscribed clients
func UpdateQueue(queue *[]models.Track, partyCode string) {
	queueJSON, _ := json.Marshal(queue)
	Event(partyCode + "queue").Submit(string(queueJSON))
}
