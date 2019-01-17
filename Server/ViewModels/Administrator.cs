using System;
using Spotify.API.NetCore.Models;

namespace Server.ViewModels
{
  public class Administrator
  {
    public PrivateProfile User { get; }
    public bool LoggedIn => User != null;

    public string Error { get; }
    public bool HasError => Error != null;

    public Image LargestUserImage {
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

    public Administrator(PrivateProfile user = null) : this(user, null)
    {
    }

    public Administrator(PrivateProfile user, string error)
    {
      User = user;
      Error = error;
    }
  }
}
