- [x] Implement custom server session frontend
- [ ] Add [QuickCheck](https://hackage.haskell.org/package/servant-quickcheck-0.0.2.2/docs/Servant-QuickCheck.html)
to the Spec tests (Which ones?)
- [x] Find/implement OAuth2 solution for Spotify authentication
- [x] Investigate [HTML datatype](https://github.com/haskell-servant/servant-lucid/blob/master/src/Servant/HTML/Lucid.hs)
- [x] Figure out if the weird session recreation because of existing authID in
packed session (serversession Core shit) is a problem **(It's not)**
- [ ] Will [file-embed](https://hackage.haskell.org/package/file-embed) be useful?

## Inline TODOs
- [ ] Use this [] for Cache-Control, HTTP-Authenticate on 401, etc. [src/Middleware/Headers.hs](src/Middleware/Headers.hs)
- [ ] Refactor this removeFlashMiddleware monstrosity [src/Middleware/Flash.hs](src/Middleware/Flash.hs)
- [ ] Refactor this auth callback monstrosity [src/Api/Auth.hs](src/Api/Auth.hs)
- [ ] Make this [Party API base URL] configurable via environment (move to Config?) [src/Utils.hs](src/Utils.hs)
- [ ] Add encompasing middleware that checks the Origin req header **TODO** [src/Middleware/Cors.hs](src/Middleware/Cors.hs)

## Compliance with Spotify Developer Terms of Use

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

### Authentication

- ~~[servant-auth](https://github.com/plow-technologies/servant-auth)~~
- ~~[authenticate-oauth](https://www.stackage.org/package/authenticate-oauth)~~

_Implemented manually in [servant-spotify](https://github.com/chances/servant-spotify#readme)._
