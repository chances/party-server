export interface IndexView {
  user: {
    id: string,
    displayName: string,
    followers: { length: number},
    images: Array<{
      url: string,
    }>,
    uri: string,
    country: string,
    product: string,
  } | null,
  currentParty: {
    roomCode: string,
  } | null,
  currentPlaylist: {
    id: number,
    name: string,
  } | null,
  playlists: Array<{id: number, owner: string, name: string, endpoint: string}> | null,
  error: string | null
}

export interface AuthView {
  username: string,
  host: string
}
