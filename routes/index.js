import express from 'express';

import {playlists, playlist, searchTracks} from '../lib/spotify';

let router = express.Router();

/* GET home page. */
router.get('/', function (req, res) {
  let pageData = {
    user: req.user,
    playlists: undefined,
    error: undefined
  };
  if (req.user && req.user.spotifyUser && req.user.accessToken) {
    req.user.currentPlaylist = null;
    playlists({
      id: req.user.spotifyUser.id,
      token: req.user.accessToken
    }, function (err, data) {
      if (err) {
        pageData.error = err;
        res.render('index', pageData);
      } else if (data.hasOwnProperty('error')) {
        pageData.error = data;
        res.render('index', pageData);
      } else {
        pageData.user = pageData.user.spotifyUser;
        pageData.playlists = data.items;
        pageData.user.currentPlaylist = req.session.currentPlaylist;
        res.render('index', pageData);
      }
    });
  } else {
    res.render('index', pageData);
  }
});

router.get('/playlist', (req, res) => {
  if (req.query.id && req.user && req.user.spotifyUser && req.user.accessToken) {
    playlist({
      userId: req.user.spotifyUser.id,
      playlistId: req.query.id,
      token: req.user.accessToken
    }, (err, data) => {
      if (!err) {
        req.session.currentPlaylist = data;

        if (req.acceptsJson) {
          res.json(data);
        } else {
          res.redirect('/');
        }
      } else {
        // TODO: Handle errors

        if (req.acceptsJson) {
          res.json(404, { error: 'Could not get playlist' });
        } else {
          res.redirect('/');
        }
      }
    });
  } else {
    if (req.acceptsJson) {
      if (!req.user) {
        res.json(401, { error: 'Unauthenticated' });
      } else {
        res.json(400, { error: 'Malformed request' });
      }
    } else {
      // TODO: Handle malformed/nonexistent query

      res.redirect('/');
    }
  }
});

router.get('/history', function (req, res, next) {
  // Get playback history (previously played songs)
  res.send('respond with a resource');
});

router.get('/queue', function (req, res, next) {
  // Get playback future (current track first?)
  // TODO: Maybe use streaming stuffs for current track/queue and history => Research
  res.send('respond with a resource');
});

router.get('/search', function (req, res, next) {
  let query = req.query.q || req.query.query || null;
  // let page = req.params.page || 0;
  if (query == null) {
    res.statusCode = 400;
    return res.json({message: 'Error 400: Malformed search, missing query'});
  }
  searchTracks({
    query: query
  }, function (err, data) {
    if (err) {
      res.statusCode = 500;
      return res.json({message: 'Error 500: ' + err.message});
    }

    res.json(data.tracks);
  });
});

export default router;
