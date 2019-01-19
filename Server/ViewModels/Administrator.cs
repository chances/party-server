using System.Collections.Generic;
using System.Linq;
using Models;
using Server.Models;
using Spotify.API.NetCore.Models;

namespace Server.ViewModels
{
  public class Administrator
  {
    public PrivateProfile User { get; }
    public bool LoggedIn => User != null;

    public Party CurrentParty { get; }
    public bool HasCurrentParty => CurrentParty != null;

    public IEnumerable<Playlist> Playlists { get; }
    public bool HasPlaylists => Playlists?.Any() ?? false;

    public string Error { get; }
    public bool HasError => Error?.Any() ?? false;

    public Image LargestUserImage
    {
      get
      {
        if (User.Images == null || User.Images.Count == 0)
        {
          return null;
        }

        var largestImage = User.Images[0];
        var size = User.Images[0].Width * User.Images[0].Height;
        foreach (var image in User.Images)
        {
          if (size < image.Width * image.Height)
          {
            largestImage = image;
          }
        }

        return largestImage;
      }
    }
    public bool HasUserImage => User.Images != null && User.Images.Count > 0;

    public Administrator(string error) : this(null, null, null, error)
    {
    }

    public Administrator(PrivateProfile user = null, Party party = null) : this(user, null, null)
    {
    }

    public Administrator(PrivateProfile user, IEnumerable<Playlist> playlists, Party party = null) : this(user, playlists, party, null)
    {
    }

    public Administrator(PrivateProfile user, IEnumerable<Playlist> playlists, Party party, string error)
    {
      User = user;
      Playlists = playlists;
      CurrentParty = party;
      Error = error;
    }
  }
}
