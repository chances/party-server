using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Extensions.Configuration;

namespace Server.Configuration
{
  /// <summary>
  /// A better implementation for <see cref="Microsoft.Extensions.Configuration.EnvironmentVariables.EnvironmentVariablesConfigurationProvider"/>
  /// </summary>
  public class OsIndependentEnvironmentVariablesConfiguration : ConfigurationProvider, IConfigurationSource
  {
    // ReSharper disable once MemberCanBePrivate.Global
    /// <summary>
    /// String comparer to find keys in the dictionary, changes this if you want to switch to case sensitive environment names.
    /// </summary>
    public StringComparer StringComparer { get; set; }

    public OsIndependentEnvironmentVariablesConfiguration()
    {
      StringComparer = StringComparer.OrdinalIgnoreCase;
    }

    /// <summary>
    /// Buil
    /// </summary>
    /// <param name="builder"></param>
    /// <returns></returns>
    public IConfigurationProvider Build(IConfigurationBuilder builder)
    {
      return this;
    }

    /// <summary>
    /// Loads in environment variables as Data dictionary
    /// </summary>
    public override void Load()
    {
      Data = new Dictionary<string, string>(StringComparer);

      var envs = Environment.GetEnvironmentVariables().Cast<DictionaryEntry>();
      foreach (var env in envs)
      {
        var key = ((string)env.Key).Replace("_", ConfigurationPath.KeyDelimiter);
        Data[key] = (string)env.Value;
      }
    }
  }

  public static class OsIndependentEnvironmentVariablesExtensions
  {
    /// <summary>
    /// Adds an <see cref="IConfigurationProvider"/> that reads configuration values from environment variables.
    /// Both : and _ are separators are
    /// </summary>
    /// <param name="configurationBuilder">The <see cref="IConfigurationBuilder"/> to add to.</param>
    /// <returns>The <see cref="IConfigurationBuilder"/>.</returns>
    public static IConfigurationBuilder AddOsIndependentEnvironmentVariables(this IConfigurationBuilder configurationBuilder)
    {
      configurationBuilder.Add(new OsIndependentEnvironmentVariablesConfiguration());
      return configurationBuilder;
    }
  }
}
