- [x] Implement custom server session frontend
- [ ] Add [QuickCheck](https://hackage.haskell.org/package/servant-quickcheck-0.0.2.2/docs/Servant-QuickCheck.html)
to the Spec tests
- [ ] Find/implement OAuth2 solution for Spotify authentication
- [ ] Investigate [HTML datatype](https://github.com/haskell-servant/servant-lucid/blob/master/src/Servant/HTML/Lucid.hs)
- [ ] Figure out if the weird session recreation because of existing authID in
packed session (serversession Core shit) is a problem
- [ ] Add encompasing middleware that checks the Origin req header __TODO__ [src/Middleware/Cors.hs](src/Middleware/Cors.hs)
- [ ] Will [file-embed](https://hackage.haskell.org/package/file-embed) be useful?

## Resource Links

### Authentication

- [servant-auth](https://github.com/plow-technologies/servant-auth)
- [authenticate-oauth](https://www.stackage.org/package/authenticate-oauth)
