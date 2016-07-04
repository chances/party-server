import express from 'express';
import passport from 'passport';
import passportSpotify from 'passport-spotify';

export default function initialize (app) {
  let base = process.env.BASE_URL || '';
  let appKey = process.env.SPOTIFY_APP_KEY;
  let appSecret = process.env.SPOTIFY_APP_SECRET;
  let appCallback = process.env.SPOTIFY_CALLBACK;

  let router = express.Router();
  let SpotifyStrategy = passportSpotify.Strategy;

  let requiredScopes = [
    'user-read-email',
    'user-read-private',
    'playlist-read-private'
  ];

  // Passport session setup.
  //   To support persistent login sessions, Passport needs to be able to
  //   serialize users into and deserialize users out of the session. Typically,
  //   this will be as simple as storing the user ID when serializing, and finding
  //   the user by ID when deserializing. However, since this example does not
  //   have a database of user records, the complete spotify profile is serialized
  //   and deserialized.
  passport.serializeUser(function (user, done) {
    done(null, user);
  });

  passport.deserializeUser(function (obj, done) {
    done(null, obj);
  });

  // Use the SpotifyStrategy within Passport.
  //   Strategies in Passport require a `verify` function, which accept
  //   credentials (in this case, an accessToken, refreshToken, and spotify
  //   profile), and invoke a callback with a user object.
  passport.use(new SpotifyStrategy({
    clientID: appKey,
    clientSecret: appSecret,
    callbackURL: appCallback
  },
  function (accessToken, refreshToken, profile, done) {
    // asynchronous verification, for effect...
    process.nextTick(function () {
      // To keep the example simple, the user's spotify profile is returned to
      // represent the logged-in user. In a typical application, you would want
      // to associate the spotify account with a user record in your database,
      // and return that user instead.
      // TODO: User persistence (also with passport user above?)
      /*
       provider: 'spotify',
       id: 'enigmaticeffigy',
       username: 'enigmaticeffigy',
       displayName: 'Chance Snow',
       profileUrl: 'https://open.spotify.com/user/enigmaticeffigy',
       photos: [ 'url' ],
       country: 'US',
       followers: 9,
       product: 'premium',
       */
      profile.meta = {
        accessToken: accessToken,
        refreshToken: refreshToken
      };
      return done(null, profile);
    });
  }));

  // Initialize Passport!  Also use passport.session() middleware, to support
  // persistent login sessions (recommended).
  app.use(passport.initialize());
  app.use(passport.session());

  // GET /auth
  //   Use passport.authenticate() as route middleware to authenticate the
  //   request. The first step in spotify authentication will involve redirecting
  //   the user to spotify.com. After authorization, spotify will redirect the user
  //   back to this application at /auth/spotify/callback
  router.get('/',
    passport.authenticate('spotify', {scope: requiredScopes, showDialog: false}),
    function (req, res) {
      // The request will be redirected to spotify for authentication, so this
      // function will not be called.
    });

  // GET /auth/callback
  //   Use passport.authenticate() as route middleware to authenticate the
  //   request. If authentication fails, the user will be redirected back to the
  //   login page. Otherwise, the primary route function function will be called,
  //   which, in this example, will redirect the user to the home page.
  router.get('/callback',
    passport.authenticate('spotify', { failureRedirect: '/' }),
    function (req, res) {
      res.redirect(base + '/');
    });

  router.get('/logout', function (req, res) {
    req.logout();
    res.redirect(base + '/');
  });

  return router;
}
