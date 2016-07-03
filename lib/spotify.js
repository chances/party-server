import * as https from 'https';

function makeResponse (callback) {
  let chunks = '';

  return function (response) {
    response.setEncoding('utf8');

    response.on('data', function (chunk) {
      chunks += chunk;
    });

    response.on('end', function () {
      let err, json;

      try {
        json = JSON.parse(chunks);
      } catch (e) {
        err = e;
        console.log(e);
      }

      callback(err, json);
    });
  };
}

export function playlists (opts, callback) {
  if (!opts.id || !opts.token) {
    throw new Error('userId and token are required');
  }
  opts.limit = opts.limit || 50;
  opts.offset = opts.offset || 0;
  let query = '/v1/users/' + opts.id + '/playlists?limit=' +
    opts.limit + '&offset=' + opts.offset;
  this.getAuth(query, opts.token, callback);
}

/**
 * Reverse-lookup a track, artist or album URI
 *
 * @param {Object} opts Options that should be used to do this query
 *                 `type` and `id` is required
 * @param {Function} callback The callback that'll be invoked once there's data
 */
export function lookup (opts, callback) {
  let type = opts.type + 's';
  let query = '/v1/' + type + '/' + opts.id;
  get(query, callback);
}

/**
 * Search the Spotify library for a track
 *
 * @param {Object} opts Options that should be used to do this query
 *                 `query` is required
 * @param {Function} callback The callback that'll be invoked once there's data
 */
export function searchTracks (opts, callback) {
  opts.limit = opts.limit || 10;
  opts.type = 'track';
  search(opts, callback);
}

/**
 * Search the Spotify library for a track, artist or album
 *
 * @param {Object} opts Options that should be used to do this query
 *                 `type` and `query` is required
 * @param {Function} callback The callback that'll be invoked once there's data
 */
export function search (opts, callback) {
  opts.limit = opts.limit || 20;
  let query = '/v1/search?type=' + opts.type + '&q=' + opts.query + '&limit=' + opts.limit;
  get(query, callback);
}

/**
 * Send a request to the Spotify web API
 *
 * @param {String} query The path for this query, see http://developer.spotify.com/en/metadata-api/overview/
 * @param {Function} callback The hollaback that'll be invoked once there's data
 */
export default function get (query, callback) {
  let opts = {
    host: 'api.spotify.com',
    path: encodeURI(query),
    method: 'GET',
    headers: { 'Accept': 'application/json' }
  };
  let request = https.request(opts, makeResponse(callback));
  request.end();

  request.on('error', function (err) {
    callback(err, {});
  });
}

/**
 * Send an authenticated request to the Spotify web API
 *
 * @param {String} query The path for this query, see http://developer.spotify.com/en/metadata-api/overview/
 * @param {String} accessToken The Spotify authorization token
 * @param {Function} callback The hollaback that'll be invoked once there's data
 */
export function getAuth (query, accessToken, callback) {
  let opts = {
    host: 'api.spotify.com',
    path: encodeURI(query),
    method: 'GET',
    headers: {
      'Accept': 'application/json',
      'Authorization': 'Bearer ' + accessToken
    }
  };
  let request = https.request(opts, makeResponse(callback));
  request.end();

  request.on('error', function (err) {
    callback(err, {});
  });
}
