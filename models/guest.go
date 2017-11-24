package models

import (
	"database/sql"
	"encoding/json"

	"github.com/vattle/sqlboiler/types"
)

type Guest struct {
	Name      string `json:"name"`
	Alias     string `json:"alias"`
	CheckedIn bool   `json:"checked_in"`
	Token     string `json:"token"`
}

type PublicGuest struct {
	Name      string `json:"name"`
	Alias     string `json:"alias"`
	CheckedIn bool   `json:"checked_in"`
}

func NewGuest(name, token string) Guest {
	return Guest{
		Name:      name,
		Alias:     "",
		CheckedIn: false,
		Token:     token,
	}
}

func (g Guest) Public() PublicGuest {
	return PublicGuest{
		Name:      g.Name,
		Alias:     g.Alias,
		CheckedIn: g.CheckedIn,
	}
}

func (o *Party) Guests() ([]Guest, error) {
	guestList, err := o.GuestG().One()
	if err != nil {
		if err != sql.ErrNoRows {
			return make([]Guest, 0), err
		}
	}
	var guests []Guest
	err = guestList.Data.Unmarshal(&guests)
	if err != nil {
		return make([]Guest, 0), err
	}
	return guests, nil
}

func (o *Party) UpdateGuestList(guests []Guest) error {
	guestsJsonRaw, _ := json.Marshal(guests)
	guestsJson := types.JSON(guestsJsonRaw)
	guestList := GuestList{
		ID:   o.GuestsID,
		Data: guestsJson,
	}
	return guestList.UpdateG()
}

func GetGuestByToken(db *sql.DB, token string) (uint32, *Guest, error) {
  query := "SELECT guests.id, guests.guest FROM " +
    "(SELECT id, json_array_elements(data) AS guest FROM guest_list)" +
      " AS guests WHERE guest->>'token'=?"
  var (
    id uint32
    guestJson []byte
  )
  row := db.QueryRow(query, token)
  err := row.Scan(&id, &guestJson)
  if err != nil {
    return 0, nil, err
  }

  var guest Guest
  err = json.Unmarshal(guestJson, &guest)
  if err != nil {
    return 0, nil, err
  }

  return id, &guest, nil
}
