using System;
using System.Text;

namespace Server.Services
{
  public class RoomCodeGenerator
  {
    private const int roomCodeLength = 4;
    private static readonly byte[] letterBytes =
      Encoding.ASCII.GetBytes("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    private static readonly int letterBytesLength = letterBytes.Length;

    private Random _random;

    public RoomCodeGenerator()
    {
      // Seed room code generator
      _random = new Random(DateTime.UtcNow.Millisecond);
      for (int i = 0; i < _random.Next(150 - 25); i += 1)
      {
        GenerateRoomCode(_random.Next(100 - 5) + 5);
      }
    }

    /// <summary>
    /// Gets a new room code.
    /// </summary>
    /// <value>The new room code.</value>
    public string NewRoomCode => GenerateRoomCode();

    /// <summary>
    /// Generates a new room code.
    /// </summary>
    /// <returns>The new room code.</returns>
    /// <param name="length">Room code length.</param>
    public string GenerateRoomCode(int length = roomCodeLength)
    {
      var b = new byte[length];
      for (var i = 0; i < length; i += 1)
      {
        var index = _random.Next(letterBytesLength);
        b[i] = letterBytes[index];
      }
      return Encoding.ASCII.GetString(b);
    }
  }
}
