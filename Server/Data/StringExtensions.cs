using System.Globalization;
using System.Text;

namespace Server.Data
{
  public static class StringExtensions
  {
    // https://github.com/JamesNK/Newtonsoft.Json/blob/master/Src/Newtonsoft.Json/Utilities/StringUtils.cs#L200
    internal enum SnakeCaseState
    {
      Start,
      Lower,
      Upper,
      NewWord
    }

    public static string ToSnakeCase(this string s)
    {
      if (string.IsNullOrEmpty(s))
      {
        return s;
      }

      StringBuilder sb = new StringBuilder();
      SnakeCaseState state = SnakeCaseState.Start;

      for (int i = 0; i < s.Length; i++)
      {
        if (s[i] == ' ')
        {
          if (state != SnakeCaseState.Start)
          {
            state = SnakeCaseState.NewWord;
          }
        }
        else if (char.IsUpper(s[i]))
        {
          switch (state)
          {
            case SnakeCaseState.Upper:
              bool hasNext = (i + 1 < s.Length);
              if (i > 0 && hasNext)
              {
                char nextChar = s[i + 1];
                if (!char.IsUpper(nextChar) && nextChar != '_')
                {
                  sb.Append('_');
                }
              }
              break;
            case SnakeCaseState.Lower:
            case SnakeCaseState.NewWord:
              sb.Append('_');
              break;
          }

          char c;
          c = char.ToLower(s[i], CultureInfo.InvariantCulture);
          sb.Append(c);

          state = SnakeCaseState.Upper;
        }
        else if (s[i] == '_')
        {
          sb.Append('_');
          state = SnakeCaseState.Start;
        }
        else
        {
          if (state == SnakeCaseState.NewWord)
          {
            sb.Append('_');
          }

          sb.Append(s[i]);
          state = SnakeCaseState.Lower;
        }
      }

      return sb.ToString();
    }
  }
}
