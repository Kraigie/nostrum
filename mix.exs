defmodule Mixcord.Mixfile do
  use Mix.Project

  def project do
    [
      app: :mixcord,
      version: "0.1.0",
      elixir: "~> 1.4",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: "An elixir Discord library",
      package: package(),
      name: "Elixir",
      source_url: "https://github.com/kraigie/mixcord",
      homepage_url: "https://github.com/kraigie/mixcord",
      deps: deps(),
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Mixcord, []}
    ]
  end

  # How to shamelessly copied from NERVES project.
  # https://github.com/nerves-project/nerves/tree/master/docs
  def docs do
    [
      output: "docs",
      main: "intro",
      extras: [
        "docs/static/Intro.md",
        "docs/static/API.md",
        "docs/static/State.md",
        "docs/static/Events.md"
      ]
    ]
  end

  def package do
    [
      name: :mixcord,
      licenses: ["MIT"],
      maintainers: ["Craig Dazey"],
      links: %{"GitHub" => "https://github.com/Kraigie/mixcord/",
              "Docs" => "https://kraigie.github.io/mixcord/"}
    ]
  end

  defp deps do
    [
      {:httpoison, "~> 0.9"},
      {:poison, "~> 3.0"},
      {:ex_doc, "~> 0.14", only: :dev},
      {:credo, "~> 0.4", only: [:dev, :test]},
      {:websocket_client, git: "https://github.com/Kraigie/websocket_client.git"},
      {:gen_stage, "~> 0.11"}
    ]
  end
end
