using System;

namespace Server.Data
{
  [AttributeUsage(AttributeTargets.Class)]
  public class ResourceIdentifierAttribute : Attribute
  {
    /// <inheritdoc />
    /// <summary>
    /// Initializes a new instance with the specified <see cref="P:Server.Data.ResourceIdentifier.Type" /> override.
    /// </summary>
    /// <param name="type">An override for a <see cref="P:Server.Data.ResourceIdentifier.Type" /></param>
    public ResourceIdentifierAttribute(string type)
    {
      Type = type;
    }

    public string Type { get; }
  }
}
