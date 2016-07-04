import db from '../lib/db';
import {migrate} from '../models';

db.sequelize
  .authenticate()
  .then(function () {
    console.log('Connection has been established successfully.');

    migrate().then(() => {
      console.log('Database has been migrated successfully.');
    });
  })
  .catch(function (err) {
    console.log('Unable to connect to the database:', err);
  });
