using Newtonsoft.Json.Linq;

namespace Server.Data
{
  public interface IResource : IResourceIdentifier
  {
    JObject Attributes { get; set; }
  }

  public interface IResource<T> : IResourceIdentifier
  {
    T Attributes { get; set; }
  }
}
