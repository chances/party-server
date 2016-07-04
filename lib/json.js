
export default function acceptsJson (req, res, next) {
  req.acceptsJson = req.accepts('html', 'json') === 'json';
  next();
}
