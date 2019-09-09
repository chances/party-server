using Newtonsoft.Json.Linq;

namespace Server.Data
{
  public interface IResourceIdentifier
  {
    string Id { get; set; }
    string Type { get; }
    JObject Metadata { get; set; }
  }
}
