- [ ] Implement custom server session backend?
- [ ] Find/implement OAuth2 solution for Spotify authentication
- [ ] Add tests with go [testing](https://golang.org/pkg/testing/) tools

- [ ] [Conditional requests](https://developer.spotify.com/web-api/user-guide/#conditional-requests) (Caching)

## Be sure to pay attention to inline TODOs in source files!

## Security

- [ ] Improve CSP support
- [ ] Add HTTPOnly or SecureOnly support to cookies
- [ ] Add CSRF token support?

## Parity with original JS prototype

- [ ] Parity with [index (unauthenticated)](https://github.com/chances/party-server/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L8)
- [ ] Spotify OAuth2 integration
- [ ] Parity with [auth routes](https://github.com/chances/party-server/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/auth.js)
- [ ] Parity with [index (authenticated)](https://github.com/chances/party-server/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L14)
  - [ ] Get user data
  - [ ] Get user's own playlists
  - [ ] Render user data
  - [ ] Render user playlists
  - [ ] Log Out
- [ ] Parity with [playlist endpoint](https://github.com/chances/party-server/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L38)
- [ ] Parity with [search endpoint](https://github.com/chances/party-server/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L71)

_Note: The Android client app authenticates with Spotify directly._

## New Functionality

- [ ] Change playlist endpoint to PATCH instead of current GET, also don't redirect
  - [ ] Update the Party's current playlist asynchronously
- [ ] Add search
  - [ ] For tracks
  - [ ] For artists
  - [ ] For albums
  - [ ] Expose "See more results in Spotify" href
  - [ ] Get access tokens for searching via [Client Credentials Flow](https://developer.spotify.com/web-api/authorization-guide/#client-credentials-flow) if session is unauthorized with Spotify

### Models

- [ ] Add Guests model
  - JSON blob (Array of):
    - Name
    - Alias (Future use?)
      - Only modifiable by a checked in Guest (guest ID/index stored in session)
    - Check-In Status (checked_in: boolean)
      - A guest is simply invited by default
      - A checked in guest is in attendance


- [ ] Add Playlist (TrackList) model
  - Fields:
    - JSON blob (Array of):
      - Spotify Track ID
      - Name
      - Artist
      - Began playing timestamp
      - Duration (in seconds)
      - Contributor name
      - Contributor ID (index in Guest array)
    - Timestamps
  - Show duration on frontend


- [ ] Add a Party Model
  - Fields:
    - Location JSON blob:
      - Host name
      - Location/address string
      - Scheduled time?
      - _Facebook integration?_
    - Room code (NULL when party has ended)
      - Random four letter combination
    - Ended flag
    - Current Track (Now Playing)
      - NULL or JSON blob:
        - Spotify Track ID
        - Name
        - Artist
        - Contributor name
        - Contributor ID (index in Guest array)
    - Queue foreign key (_To TrackList_)
    - History foreign key (_To TrackList_)
    - Guests foreign key

### Party Access Tokens

_Party Access Tokens_ authenticate API access for party guests. (Party hosts authenticate separately with Spotify.)

- [ ] Add access token delivery
  - [ ] Sign generated tokens using a strong signature (token secret, cryptographic hash)
  - [ ] Store tokens in Redis
    - No persistence necessary for guest tokens, parties are ephemeral
  - [ ] Deliver token inside a secure cookie
  - [ ] Deliver a _Party Access Token_ to pseudo-authenticated guests
    - Those who have joined a party with valid party ID **and** over SSL **_with_** CORS Origin validation
    - Store originating Origin and validate subsequent requests given the request's _Party Access Token_
  - [ ] Recurring job to clean expired tokens
    - Party access tokens expire after 30 minutes of disuse
  - [ ] Automatically refresh token expiration time via ping/pong

### Server Events

- [ ] Server-Sent Events data integration for party state updates sent to clients
  - Push TrackList and Party model updates to guests
  - Gin comes [ready built for SSE](https://github.com/gin-gonic/gin/tree/2dae550eb5392006a4582ce9c90016a9b5a74e8b/examples/realtime-chat)
  - [go-broadcast](https://github.com/dustin/go-broadcast)

- [ ] Scalable Server-Sent Events data integration via Redis PubSub
  - Current implementation keeps connection info in memory
  - This will not scale to multiple Heroku dynos

- [ ] ~~WebSockets streaming data integration?~~
  - Do I need full duplex send/receive sockets? **Probably not in most cases**
  - _Future?_


- [ ] Playback history (History) endpoint _(As TrackList)_
  - [ ] History endpoint
  - [ ] Pagination
    - 20 tracks, by default, per page
    - `limit` and `offset` query params
  - [ ] With SSE support (Sent via party, `/events/party`, stream as `history` event)
    - Updates when current track changes
  - Most recent track first
  - Clients may use a track's `began_playing` timestamp to show timeago info
- [ ] Playback future (Queue) endpoint _(As TrackList)_
  - [ ] Queue endpoint
  - [ ] Pagination
    - 10 tracks, by default, per page
    - `limit` and `offset` query params
  - [ ] With SSE support (Sent via party, `/events/party`, stream as `queue` event)
    - Updates when current track changes
    - Updates when guests contribute tracks
  - Server keeps track of whole Queue
    - Hosts may view whole queue
    - Guests may only see first page of queue (First page w/ default page size; ignore page query params)
- [ ] Add Party endpoints
  - [ ] Create (Start?)
  - [ ] End (Stop/Quit?)
  - [ ] Get current party
  - [ ] Join a party
    - [ ] Via _authorized_ room code
    - [ ] Check-In via OAuth provider
    - [ ] With SSE support (Update clients with Guest-list)
  - [ ] Connect to Facebook event (_Future?_)
- [ ] Add search suggestions (autocomplete)?
  - _Maybe better with a WebSocket?_

### Party Host Features

- [ ] Mobile app authentication
  - [ ] Open `/auth/login` in a web view
  - [ ] Redirect back to new `/auth/finished` endpoint
    - Render: `Logged in as $name$ : <a href=/auth/logout>Logout</a>`
    - Mobile app handles redirection back here
  - [ ] Spotify access token delivery
    - Only deliver access token, server manages refresh
  - [ ] Spotify token refresh endpoint `/auth/refresh`
    - Responds with refreshed token

- [ ] Playback
  - [ ] Pick a playlist for a party
    - [ ] Pull all of the playlist's tracks
    - [ ] [Shuffle](https://labs.spotify.com/2014/02/28/how-to-shuffle-songs/) the playlist
  - [ ] Play the party's playlist
  - [ ] Pause the party's playlist
  - [ ] Skip the current track
  - [ ] Search to add to queue
  - [ ] State change notifications (WebSockets?)

### Party Guest Features

- [ ] Add _Join Party_ endpoint
  - With Party foreign key
  - Should there be a party list? **No**
  - User facing "room" code for guests to enter (Like Jackbox. _456,976 combinations with 4 letters_, but I'm using any alpha-numeric combination of four characters, _13,845,841 possible combinations_)
  - Facebook integration (_Future_)
- [ ] Contribution
  - [ ] Add (suggest) a song
  - [ ] Limit maximum number of suggestions per timeframe (hour?)
  - [ ] Suggest that a song should be skipped
    - [ ] Require a minimum number of votes (5? fraction of party attendants?)
- [ ] Join a party given its room code
  - _unauthenticated_ ⇒ _pseudo-authenticated_ (Via room code authorization)
- [ ] Check-In to a Party (_authenticated_)
  - [ ] OAuth2 third-party authentication schemes
    - [ ] Facebook
    - [ ] Google
    - [ ] Twitter
    - [ ] GitHub
    - Others?

#### Wishlist

- [ ] Music Quiz modeled after [Spotify iQuiz](https://github.com/JMPerez/spotify-iquiz#spotify-iquiz)
  - Users can submit songs, albums, and artists for inclusion to the communal quiz game beforehand.
  - Display a call to action on the TV UI and to guests that they may participate.
  - Prevent users from selecting songs from singles or hide the album art (Singles are too easy to guess given their album art).

## Compliance with Spotify Developer Terms of Use

- [Spotify Developer Terms of Use](https://developer.spotify.com/developer-terms-of-use/)
- [Design Guidelines](https://developer.spotify.com/design/)

### Specific sections

- Section V: Users & Data
  - [ ] Obtain explicit consent for users' email address?
    - "5. You shall not email Spotify users unless you obtain their explicit consent or obtain their email address and permission through means other than Spotify."

  - [ ] Delete all Spotify Content upon logout/inactivity
    - "7. Spotify user data can be cached only for operating your SDA. If a Spotify user logs out of your SDA or becomes inactive, you will delete any Spotify Content related to that user stored on your servers. To be clear, you are not permitted to store Spotify Content related to a Spotify user or otherwise request user data if a Spotify user is not using your SDA."

  - [ ] Add 'Delete Account' functionality.
    - "8. You must provide all users with a working mechanism to disconnect their Spotify Account from your application at any time and provide clear instructions on how to do so. Further, when a user disconnects their Spotify account, you agree to delete and no longer request or process any of that Spotify user’s data."

  - [ ] Write an **EULA/Terms of Use** and a **Privacy Policy** for Party
    - "10. You must have an end user agreement and privacy policy. Any access, use, processing, and disclosure of Spotify user data shall comply with (i) these Developer Terms; (ii) your end user license agreement; (iii) your privacy policy; and (iv) applicable laws and regulations."
    - See subsection 12 about requirements for EULA/Terms of Use
    - See subsection 13 about requirements for Privacy Policy

## Resource Links

- [spotify-web-api-js](https://github.com/JMPerez/spotify-web-api-js)

### WebSockets

_TODO: Get resources for Go/TypeScript WebSockets_

### Authentication

_TODO: Get resources for Go OAuth providers (Facebook, Google, Twitter, GitHub, etc.) and super pretty frontend login buttons_
