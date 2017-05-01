- [ ] Implement custom server session backend, see [gin-contrib/sessions](https://github.com/gin-contrib/sessions)
- [ ] Find/implement OAuth2 solution for Spotify authentication
- [ ] Add tests with go [testing](https://golang.org/pkg/testing/) tools
- [ ] Switch to [Makefile based](https://github.com/chances/heroku-buildpack-go/tree/add-make-tool) Heroku deploy?
- [ ] [Conditional requests](https://developer.spotify.com/web-api/user-guide/#conditional-requests) (Caching)

## Be sure to pay attention to inline TODOs in source files!

## Security

- [ ] Improve CSP support
- [ ] Add HTTPOnly or SecureOnly support to cookies
- [ ] Add CSRF token support?

## Parity with original JS prototype

- [ ] Parity with [index (unauthenticated)](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L8)
- [ ] Spotify OAuth2 integration
- [ ] Parity with [auth routes](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/auth.js)
- [ ] Parity with [index (authenticated)](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L14)
  - [ ] Get user data
  - [ ] Get user's own playlists
  - [ ] Render user data
  - [ ] Render user playlists
  - [ ] Log Out
- [ ] Parity with [playlist endpoint](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L38)
- [ ] ~~Parity with [search endpoint](https://github.com/chances/chances-party/blob/94ce862cb8fc9ef94b3b8c73c404479c3d86e659/routes/index.js#L71)~~
  - [ ] ~~Add search to [servant-spotify](https://github.com/chances/servant-spotify#readme)~~
    - [ ] ~~For tracks~~
    - [ ] ~~For artists~~
    - [ ] ~~For albums~~
    - [ ] ~~Expose "See more results in Spotify" href~~

_Note: Removed search endpoint task in favor of access token delivery to clients for use with the Spotify Web API on the client side. (The Android client app authenticates with Spotify directly.)_

## New Functionality

- [ ] Add access token delivery endpoint
  - [ ] Deliver token to authenticated sessions
  - [ ] Deliver token to authenticated sessions on same root domain
  - [ ] Deliver token to guests via the party's host's user
  - [ ] Automatically refresh token
- [ ] Remove user[s] endpoints because they're gaping security holes
- [ ] Change playlist to PUT instead of current GET, also don't redirect
- [ ] Add timestamp utility functions (autonomously update them)
- [ ] WebSockets streaming data integration
- [ ] Add Guests model
  - JSON blob (Array of):
    - Name
    - Alias (Future use?)
      - Only modifiable by a checked in Guest (guest ID/index stored in session)
    - Check-In Status (_A guest is simply invited by default. A checked in guest is in attendance._)
- [ ] Add Playlist (TrackList) model
  - Fields:
    - JSON blob (Array of):
      - Spotify Track ID
      - Name
      - Artist
      - Contributor name
      - Contributor ID (index in Guest array)
    - Timestamps
- [ ] Add History model
  - Fields:
    - TrackList foreign key
    - Began playing timestamp
    - Timestamps
  - Show duration on frontend?
- [ ] Playback history (History) endpoint _(As TrackList)_
  - [ ] With WebSocket support
- [ ] Add Queue model
  - TrackList foreign key
  -
- [ ] Playback future (Queue) endpoint _(As TrackList)_
  - [ ] With WebSocket support
- [ ] Add a Party Model
  - Fields:
    - Host name (Location/address, scheduled time too? _Facebook inegration?_)
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
    - Queue foreign key
    - History foreign key
    - Guests foreign key
- [ ] Add Party endpoints
  - [ ] Create
  - [ ] End (Stop/Quit?)
  - [ ] Connect to Facebook event (_Future?_)
- [ ] Add search suggestions (while typing query)?

### Party Host Features

- [ ] Playback
  - [ ] Pick a playlist for a party
  - [ ] Play/pause the party's playlist
  - [ ] Skip the current track
  - [ ] Search to add to queue
  - [ ] State change notifications (WebSockets?)

### Party Guest Features

- [ ] Add Join Party endpoint
  - With Party foreign key
  - Should there be a party list? **No**
  - User facing "room" code for guests to enter (Like Jackbox. _456,976 combinations with 4 letters_)
  - Facebook integration (_Future_)
- [ ] Contribution
  - [ ] Add (suggest) a song
    - [ ] Limit to a maximum limit per timeframe (hour?)
  - [ ] Suggest that a song should be skipped
    - [ ] Require a minimum number of votes (5? fraction of party attendants?)
- [ ] Join a party (_unauthenticated_)
- [ ] Check-In to a Party (_authenticated_)
  - [ ] OAuth2 third-party login schemes
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

- [socket-io](http://hackage.haskell.org/package/socket-io)
- [websockets](https://github.com/jaspervdj/websockets)
- [servant-subscriber](https://github.com/eskimor/servant-subscriber)
- [ChatQY](https://github.com/realli/chatqy) (servant, websockets, and react.js)

### Authentication

- ~~[servant-auth](https://github.com/plow-technologies/servant-auth)~~
- ~~[authenticate-oauth](https://www.stackage.org/package/authenticate-oauth)~~

_Implemented manually in [servant-spotify](https://github.com/chances/servant-spotify#readme)._
