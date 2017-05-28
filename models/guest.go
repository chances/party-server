package models

type Guest struct {
	Name      string `json:"name"`
	Alias     string `json:"alias"`
	CheckedIn bool   `json:"checked_in"`
}

func NewGuest(name string) Guest {
	return Guest{
		Name:      name,
		Alias:     "",
		CheckedIn: false,
	}
}
