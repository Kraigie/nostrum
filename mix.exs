defmodule Nostrum.Mixfile do
  @moduledoc false
  use Mix.Project

  def project do
    [
      app: :nostrum,
      version: "0.4.5",
      elixir: "~> 1.9",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      description: "An elixir Discord library",
      package: package(),
      name: "Nostrum",
      source_url: "https://github.com/kraigie/nostrum",
      homepage_url: "https://github.com/kraigie/nostrum",
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
      extra_applications: [:logger, :inets],
      mod: {Nostrum.Application, []}
    ]
  end

  # "How to" shamelessly copied from the nerves project.
  # https://github.com/nerves-project/nerves/tree/master/docs
  def docs do
    [
      main: "intro",
      extras: extras(),
      groups_for_modules: groups_for_modules()
    ]
  end

  def extras do
    [
      "docs/static/Intro.md",
      "docs/static/API.md",
      "docs/static/State.md",
      "docs/static/Events.md",
      "docs/static/Consumers.md",
      "docs/static/Voice.md",
      "docs/static/Gateway Intents.md",
      "docs/static/Slash commands.md"
    ]
  end

  def groups_for_modules do
    [
      Api: [
        ~r/Nostrum.Api/
      ],
      Cache: [
        ~r/Nostrum.Cache/
      ],
      Structs: [
        ~r/Nostrum.Struct/
      ]
    ]
  end

  def aliases do
    [
      lint: ["format --check-formatted", "credo --strict"]
    ]
  end

  def package do
    [
      name: :nostrum,
      licenses: ["MIT"],
      maintainers: ["Craig Dazey"],
      links: %{
        "GitHub" => "https://github.com/Kraigie/nostrum/",
        "Docs" => "https://kraigie.github.io/nostrum/"
      }
    ]
  end

  defp deps do
    [
      {:httpoison, "~> 1.7"},
      {:poison, "~> 3.0"},
      {:gun, "~> 1.3"},
      {:kcl, "~> 1.3"},
      {:porcelain, "~> 2.0"},
      {:ex_doc, "~> 0.14", only: :dev},
      {:credo, "~> 1.4", only: [:dev, :test]},
      {:dialyxir, "~> 1.0.0", only: [:dev], runtime: false},
      {:gen_stage, "~> 0.11 or ~> 1.0"},
      {:recon, "~> 2.3", only: :dev}
    ]
  end

  def dialyzer do
    [
      plt_add_deps: :transitive,
      plt_add_apps: [:mix]
    ]
  end
end
