let models = {};

if (!global.hasOwnProperty('db')) {
  throw new Error('Database connection has not been established');
} else {
  let sequelize = global.db.sequelize;

  models = {
    User: sequelize.import(__dirname + '/user')
    // add your other models here
  };

  global.db.models = models;

  /*
   Associations can be defined here. E.g. like this:
   global.db.User.hasMany(global.db.SomethingElse)
   */

  // Sync the models
  global.db.sequelize.sync();
}

export default models;

export function migrate () {
  // Sync the models, forcefully for the janky migration
  return global.db.sequelize.sync({ force: true });
}
