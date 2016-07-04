import express from 'express';
import * as path from 'path';
import logger from 'morgan';
import session from 'express-session';
import connectSqlite from 'connect-sqlite3';
import bodyParser from 'body-parser';

import cors from './lib/cors';
import acceptsJson from './lib/json';
import db from './lib/db';

import routes from './routes/index';
import {default as authInit} from './routes/auth';
import users from './routes/users';

let app = express();

let SQLiteStore = connectSqlite(session);

db.sequelize
  .authenticate()
  .then(function () {
    console.log('Connection has been established successfully.');
  })
  .catch(function (err) {
    console.log('Unable to connect to the database:', err);
  });

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
app.use(acceptsJson);

app.use(express.static(__dirname + '/public'));

let auth = authInit(app);

app.use('/', routes);
app.use('/auth', auth);
app.use('/users', users);

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
