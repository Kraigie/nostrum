defmodule Nostrum.Mixfile do
  use Mix.Project

  def project do
    [
      app: :nostrum,
      version: "0.2.1",
      elixir: "~> 1.4",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: "An elixir Discord library",
      package: package(),
      name: "Elixir",
      source_url: "https://github.com/kraigie/nostrum",
      homepage_url: "https://github.com/kraigie/nostrum",
      deps: deps(),
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Nostrum, []}
    ]
  end

  # How to shamelessly copied from NERVES project.
  # https://github.com/nerves-project/nerves/tree/master/docs
  def docs do
    [
      main: "intro",
      extras: [
        "docs/static/Intro.md",
        "docs/static/API.md",
        "docs/static/State.md",
        "docs/static/Events.md",
        "docs/static/Consumers.md"
      ]
    ]
  end

  def package do
    [
      name: :nostrum,
      licenses: ["MIT"],
      maintainers: ["Craig Dazey"],
      links: %{"GitHub" => "https://github.com/Kraigie/nostrum/",
              "Docs" => "https://kraigie.github.io/nostrum/"}
    ]
  end

  defp deps do
    [
      {:httpoison, "== 0.11.1"},
      {:poison, "~> 3.0"},
      {:ex_doc, "~> 0.14", only: :dev},
      {:credo, "~> 0.4", only: [:dev, :test]},
      {:gun, "~> 1.0.0-pre.2"},
      {:gen_stage, "~> 0.11"}
    ]
  end
end
