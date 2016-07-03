import express from 'express';
import * as path from 'path';
import logger from 'morgan';
import session from 'express-session';
import connectSqlite from 'connect-sqlite3';
import bodyParser from 'body-parser';

import cors from './lib/cors';

import routes from './routes/index';
import auth from './routes/auth';
import users from './routes/users';

let app = express();

let SQLiteStore = connectSqlite(session);

// view engine setup
app.set('views', path.join(__dirname, '../views'));
app.set('view engine', 'ejs');

app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(session({
  store: new SQLiteStore(),
  secret: process.env.SESSION_SECRET || 'reallyBadSecret',
  cookie: { maxAge: 24 * 60 * 60 * 1000 }, // 1 day
  resave: false,
  saveUninitialized: false
}));

app.use(cors);

app.use(express.static(__dirname + '/public'));

let base = process.env.BASE_URL || '';

app.use(base + '/', routes);
app.use(base + '/auth', auth(app));
app.use(base + '/users', users);

// catch 404 and forward to error handler
app.use(function (req, res, next) {
  let err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handlers

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
  app.use(function (err, req, res, next) {
    res.status(err.status || 500);
    res.json({
      message: err.message,
      stackTrace: err.stack,
      error: err
    });
  });
}

// production error handler
// no stacktraces leaked to user
app.use(function (err, req, res, next) {
  res.status(err.status || 500);
  res.json({
    message: err.message,
    error: {}
  });
});

export default app;
