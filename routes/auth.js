var express = require('express');
var router = express.Router();
var passport = require('passport');
var SpotifyStrategy = require('passport-spotify').Strategy;

function initialize(app) {

  var appKey = process.env.SPOTIFY_APP_KEY;
  var appSecret = process.env.SPOTIFY_APP_SECRET;

  // Passport session setup.
  //   To support persistent login sessions, Passport needs to be able to
  //   serialize users into and deserialize users out of the session. Typically,
  //   this will be as simple as storing the user ID when serializing, and finding
  //   the user by ID when deserializing. However, since this example does not
  //   have a database of user records, the complete spotify profile is serialized
  //   and deserialized.
  passport.serializeUser(function(user, done) {
    done(null, user);
  });

  passport.deserializeUser(function(obj, done) {
    done(null, obj);
  });

  // Use the SpotifyStrategy within Passport.
  //   Strategies in Passport require a `verify` function, which accept
  //   credentials (in this case, an accessToken, refreshToken, and spotify
  //   profile), and invoke a callback with a user object.
  passport.use(new SpotifyStrategy({
      clientID: appKey,
      clientSecret: appSecret,
      callbackURL: 'http://localhost:3005/auth/callback'
    },
    function(accessToken, refreshToken, profile, done) {
      // asynchronous verification, for effect...
      process.nextTick(function () {
        // To keep the example simple, the user's spotify profile is returned to
        // represent the logged-in user. In a typical application, you would want
        // to associate the spotify account with a user record in your database,
        // and return that user instead.
        return done(null, profile);
      });
    }));
}

module.exports = {
  init: initialize,
  router: router
};
