// CORS with chancesnow.me or specific origin middleware
let allowCrossDomain = function (req, res, next) {
  res.header('Access-Control-Allow-Origin', process.env.CORS_ORIGIN || 'http://chancesnow.me');
  res.header('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE');
  res.header('Access-Control-Allow-Headers', 'Content-Type');

  next();
};

export default allowCrossDomain;
