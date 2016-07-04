import * as path from 'path';
import Sequelize from 'sequelize';

let sequelize = null;

if (process.env.DATABASE_URL) {
  sequelize = new Sequelize(process.env.DATABASE_URL);
} else {
  sequelize = new Sequelize('party', null, null, {
    dialect: 'sqlite',

    storage: path.join('party.db')
  });
}

export default sequelize;
