import * as path from 'path';
import Sequelize from 'sequelize';

let sequelize = null;

if (!global.hasOwnProperty('db')) {
  if (process.env.DATABASE_URL) {
    sequelize = new Sequelize(process.env.DATABASE_URL);
  } else {
    sequelize = new Sequelize('party', null, null, {
      dialect: 'sqlite',

      storage: path.join('party.db')
    });
  }

  global.db = {
    Sequelize: Sequelize,
    sequelize: sequelize
  };
}

export default global.db;
