var express = require('express');
var router = express.Router();

/* GET home page. */
router.get('/', function (req, res, next) {
  res.render('index', { title: 'Express' });
});

router.get('/history', function (req, res, next) {
  // Get playback history (previously played songs)
  res.send('respond with a resource');
});

router.get('/queue', function (req, res, next) {
  // Get playback future
  res.send('respond with a resource');
});

router.get('/search', function (req, res, next) {
  res.send('respond with a resource');
});

module.exports = router;
