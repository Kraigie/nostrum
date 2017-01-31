defmodule Mixcord.Mixfile do
  use Mix.Project

  def project do
    [
      app: :mixcord,
      version: "0.1.0",
      elixir: "~> 1.3",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      docs: docs()
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [
      extra_applications: [:logger],
      mod: {Mixcord, []}
    ]
  end

  def docs do
    [
      output: "docs"
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:httpoison, "~> 0.9"},
      {:poison, "~> 3.0"},
      {:ex_doc, "~> 0.12", only: :dev},
      {:credo, "~> 0.4", only: [:dev, :test]},
      {:websocket_client, git: "https://github.com/Kraigie/websocket_client.git"}
    ]
  end
end
