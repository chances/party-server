using System.Collections.Generic;
using System.Linq;
using Spotify.API.NetCore.Models;

namespace Server.Services.Spotify
{
  public static class ImageExtensions
  {
    public static Image GetLargestImage(this PrivateProfile profile)
    {
      if (profile.Images == null || profile.Images.Count == 0)
      {
        return null;
      }

      var largestImage = profile.Images[0];
      var size = profile.Images[0].Width * profile.Images[0].Height;
      foreach (var image in profile.Images)
      {
        if (size < image.Width * image.Height)
        {
          largestImage = image;
        }
      }

      return largestImage;
    }

    public static Image GetLargestImage(this PublicProfile profile)
    {
      if (profile.Images == null || profile.Images.Count == 0)
      {
        return null;
      }

      var largestImage = profile.Images[0];
      var size = profile.Images[0].Width * profile.Images[0].Height;
      foreach (var image in profile.Images)
      {
        if (size < image.Width * image.Height)
        {
          largestImage = image;
        }
      }

      return largestImage;
    }
  }
}
