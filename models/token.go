package models

import "time"

type SpotifyToken struct {
	AccessToken string    `json:"access_token"`
	TokenExpiry time.Time `json:"token_expiry"`
}
