var https = require('https');

function makeResponse (callback) {
  var chunks = '';

  return function (response) {
    response.setEncoding('utf8');

    response.on('data', function (chunk) {
      chunks += chunk;
    });

    response.on('end', function () {
      var err, json;

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

module.exports = {
  /**
   * Reverse-lookup a track, artist or album URI
   *
   * @param {Object} opts Options that should be used to do this query
   *                 `type` and `id` is required
   * @param {Function} callback The callback that'll be invoked once there's data
   */
  lookup: function (opts, callback) {
    var type = opts.type + 's';
    var query = '/v1/' + type + '/' + opts.id;
    this.get(query, callback);
  },

  /**
   * Search the Spotify library for a track
   *
   * @param {Object} opts Options that should be used to do this query
   *                 `query` is required
   * @param {Function} callback The callback that'll be invoked once there's data
   */
  searchTracks: function (opts, callback) {
    opts.limit = opts.limit || 10;
    opts.type = 'track';
    this.search(opts, callback);
  },

  /**
   * Search the Spotify library for a track, artist or album
   *
   * @param {Object} opts Options that should be used to do this query
   *                 `type` and `query` is required
   * @param {Function} callback The callback that'll be invoked once there's data
   */
  search: function (opts, callback) {
    opts.limit = opts.limit || 20;
    var query = '/v1/search?type=' + opts.type + '&q=' + opts.query + '&limit=' + opts.limit;
    this.get(query, callback);
  },

  /**
   * Send a request to the Spotify web service API
   *
   * @param {String} query The path for this query, see http://developer.spotify.com/en/metadata-api/overview/
   * @param {Function} callback The hollaback that'll be invoked once there's data
   */
  get: function (query, callback) {
    var opts = {
      host: 'api.spotify.com',
      path: encodeURI(query),
      method: 'GET',
      headers: { 'Accept': 'application/json' }
    };
    var request = https.request(opts, makeResponse(callback));
    request.end();

    request.on('error', function (err) {
      callback(err, {});
    });
  }
};
