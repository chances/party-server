
export default function User (sequelize, DataTypes) {
  return sequelize.define('User', {
    username: {
      type: DataTypes.STRING,
      allowNull: false
    },
    spotifyUser: {
      type: DataTypes.TEXT,
      field: 'spotify_user',
      allowNull: false
    },
    accessToken: {
      type: DataTypes.TEXT,
      field: 'access_token',
      allowNull: false
    },
    refreshToken: {
      type: DataTypes.TEXT,
      field: 'refresh_token',
      allowNull: false
    }
  });
}
