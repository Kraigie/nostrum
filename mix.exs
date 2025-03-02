defmodule Nostrum.Mixfile do
  @moduledoc false
  use Mix.Project

  def project do
    [
      app: :nostrum,
      appup: "appup.ex",
      compilers: Mix.compilers() ++ [:appup],
      version: "0.10.4",
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: "An Elixir Discord library",
      package: package(),
      name: "Nostrum",
      source_url: "https://github.com/Kraigie/nostrum",
      homepage_url: "https://github.com/Kraigie/nostrum",
      deps: deps(),
      docs: docs(),
      dialyzer: dialyzer(),
      aliases: aliases()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [
      extra_applications: [:logger, :inets, mnesia: :optional],
      mod: {Nostrum.Application, []}
    ]
  end

  # "How to" shamelessly copied from the nerves project.
  # https://github.com/nerves-project/nerves/tree/master/docs
  def docs do
    [
      main: "intro",
      extras: extras(),
      logo: "assets/icon.png",
      groups_for_modules: groups_for_modules(),
      groups_for_docs: groups_for_docs(),
      groups_for_extras: [
        Introduction: ~r"/introduction/",
        Functionality: ~r"/functionality/",
        Configuration: ~r"/configuration/",
        Advanced: ~r"/advanced/",
        "Cheat Sheets": ~r"/cheat-sheets/"
      ],
      source_ref: "master",
      assets: "guides/assets",
      nest_modules_by_prefix: [
        Nostrum.Cache,
        Nostrum.Cache.ChannelGuildMapping,
        Nostrum.Cache.GuildCache,
        Nostrum.Cache.MemberCache,
        Nostrum.Cache.PresenceCache,
        Nostrum.Cache.UserCache,
        Nostrum.Constants,
        Nostrum.Store,
        Nostrum.Store.GuildShardMapping,
        Nostrum.Store.UnavailableGuild,
        Nostrum.Struct,
        Nostrum.Struct.Event
      ]
    ]
  end

  def extras do
    # We manually move the `intro.md` document to the top of the list so it
    # remains first on the navigation sidebar
    ["guides/intro/intro.md"] ++
      (Path.wildcard("guides/**/*.{md,cheatmd}") -- ["guides/intro/intro.md"])
  end

  def groups_for_modules do
    [
      Api: [
        ~r/Nostrum.Api$/,
        ~r/Nostrum.Consumer/,
        ~r/Nostrum.(Permission|Voice)/
      ],
      Cache: [
        ~r/Nostrum.Cache.\w+$/
      ],
      Structs: [
        ~r/Nostrum.Struct/
      ],
      Constants: [
        ~r/Nostrum.Constants/
      ],
      Utilities: [
        ~r/Nostrum.(Snowflake|Token|Util)/
      ],
      Stores: [
        ~r/Nostrum.Store.\w+$/
      ],
      "Pluggable caches & stores": [
        ~r/Nostrum.Cache.\w+.\w+$/,
        ~r/Nostrum.Store.\w+.\w+$/
      ],
      "Internal modules": [
        ~r/Nostrum.Api.Ratelimiter/,
        ~r/Nostrum.Shard/,
        ~r/^Nostrum\.StateMachineTranslator$/
      ]
    ]
  end

  defp groups_for_docs,
    do: []

  def aliases do
    [
      lint: ["format --check-formatted", "credo --strict"]
    ]
  end

  def package do
    [
      name: :nostrum,
      licenses: ["MIT"],
      files: [
        "appup.ex",
        "examples",
        "lib",
        ".formatter.exs",
        "mix.exs",
        "README*",
        "LICENSE*",
        "src"
      ],
      maintainers: ["Craig Dazey", "Johannes Christ", "Joe Banks"],
      links: %{
        "GitHub" => "https://github.com/Kraigie/nostrum/",
        "Docs" => "https://kraigie.github.io/nostrum/"
      }
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},
      {:gun, "~> 2.0"},
      {:certifi, "~> 2.13"},
      {:mime, "~> 1.6 or ~> 2.0"},
      {:ezstd, "~> 1.1", optional: true},
      {:castle, "~> 0.3.0", runtime: false},
      {:ex_doc, "~> 0.32", only: :dev, runtime: false},
      {:credo, "~> 1.7.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.1", only: [:dev], runtime: false},
      {:benchee, "~> 1.1", only: :dev, runtime: false},
      {:recon, "~> 2.3", only: :dev, optional: true}
    ]
  end

  def dialyzer do
    [
      plt_add_deps: :transitive,
      plt_add_apps: [:mix, :mnesia],
      # "-Wmissing_return"]
      flags: ["-Wextra_return", "-Werror_handling"]
    ]
  end
end
