- [x] Implement custom server session backend, see [gin-contrib/sessions](https://github.com/gin-contrib/sessions)
- [x] Find/implement OAuth2 solution for Spotify authentication
- [ ] Add tests with go [testing](https://golang.org/pkg/testing/) tools
- [ ] Add the Node.js buildpack to build Node.js assets

  See: [Deploying Golang app with Bower on Heroku](http://stackoverflow.com/a/33387855/1363247)

- [ ] [Conditional requests](https://developer.spotify.com/web-api/user-guide/#conditional-requests) (Caching)

## Be sure to pay attention to inline TODOs in source files!

## Security

- [ ] Improve CSP support
- [x] Add HTTPOnly or SecureOnly support to cookies
- [ ] Add CSRF token support?

## Parity with original JS prototype

- [x] Parity with [index (unauthenticated)](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L8)
- [x] Spotify OAuth2 integration
- [x] Parity with [auth routes](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/auth.js)
- [x] Parity with [index (authenticated)](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L14)
  - [x] Get user data
  - [x] Get user's own playlists
  - [x] Render user data
  - [x] Render user playlists
  - [x] Log Out
- [x] Parity with [playlist endpoint](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L38)
- [x] Parity with [search endpoint](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L71)

_Note: The Android client app authenticates with Spotify directly._

## New Functionality

- [x] Change playlist endpoint to PATCH instead of current GET, also don't redirect
  - [x] Update the Party's current playlist asynchronously
- [ ] Add search
  - [x] For tracks
  - [ ] For artists
  - [ ] For albums
  - [ ] Expose "See more results in Spotify" href
  - [x] Get access tokens for searching via [Client Credentials Flow](https://developer.spotify.com/web-api/authorization-guide/#client-credentials-flow) if session is unauthorized with Spotify

### Models

- [x] Add Guests model
  - JSON blob (Array of):
    - Name
    - Alias (Future use?)
      - Only modifiable by a checked in Guest (guest ID/index stored in session)
    - Check-In Status (checked_in: boolean)
      - A guest is simply invited by default
      - A checked in guest is in attendance


- [x] Add Playlist (TrackList) model
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


- [x] Add a Party Model
  - Fields:
    - Location JSON blob:
      - Host name
      - Location/address string
      - Scheduled time?
      - _Facebook inegration?_
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
  - [x] Store tokens in Redis
    - No persistence necessary for guest tokens, parties are ephemeral
  - [x] Deliver token inside a secure cookie
  - [x] Deliver a _Party Access Token_ to pseudo-authenticated guests
    - Those who have joined a party with valid party ID **and** over SSL **_with_** CORS Origin validation
    - Store originating Origin and validate subsequent requests given the request's _Party Access Token_
  - [ ] Recurring job to expire and clean expired tokens
    - Party access tokens expire after 30 minutes of disuse
  - [ ] Automatically refresh token expiration time via ping/pong

### Server Events

- [ ] Server-Sent Events data integration for party state updates sent to clients
  - Push TrackList and Party model updates to guests
  - Gin comes [ready built for SSE](https://github.com/gin-gonic/gin/tree/2dae550eb5392006a4582ce9c90016a9b5a74e8b/examples/realtime-chat)
  - [go-broadcast](https://github.com/dustin/go-broadcast)


- [ ] ~~WebSockets streaming data integration?~~
  - Do I need full duplex send/receive sockets? **Probably not in most cases**
  - _Future?_


- [ ] Playback history (History) endpoint _(As TrackList)_
  - [ ] With SSE support
- [ ] Playback future (Queue) endpoint _(As TrackList)_
  - [ ] With SSE support
- [ ] Add Party endpoints
  - [x] Create (Start?)
  - [ ] End (Stop/Quit?)
  - [x] Get current party
  - [ ] Join a party
    - Via _pseudo-authenticated_ room code, delivering JWT, or
    - Check-In via OAuth provider
    - [ ] With SSE support (Update clients with Guest-list)
  - [ ] Connect to Facebook event (_Future?_)
- [ ] Add search suggestions (autocomplete)?
  - _Maybe better with a WebSocket?_

### Party Host Features

- [ ] Playback
  - [ ] Pick a playlist for a party
  - [ ] Play/pause the party's playlist
  - [ ] Skip the current track
  - [ ] Search to add to queue
  - [ ] State change notifications (WebSockets?)

### Party Guest Features

- [ ] Add _Join Party_ endpoint
  - With Party foreign key
  - Should there be a party list? **No**
  - User facing "room" code for guests to enter (Like Jackbox. _456,976 combinations with 4 letters_)
  - Facebook integration (_Future_)
- [ ] Contribution
  - [ ] Add (suggest) a song
    - [ ] Limit to a maximum limit per timeframe (hour?)
  - [ ] Suggest that a song should be skipped
    - [ ] Require a minimum number of votes (5? fraction of party attendants?)
- [ ] Join a party
  - _unauthenticated_ ⇒ _pseudo-authenticated_ (Via JWT)
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
