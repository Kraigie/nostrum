# Mixcord

Mixcord is a wrapper for the Discord API made in [Elixir](http://elixir-lang.org/).

This is more of a project for me to learn Elixir than it is to be used widely. Feel free to contribute.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed as:

  1. Add `mixcord` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:mixcord, "~> 0.1.0"}]
    end
    ```

  2. Ensure `mixcord` is started before your application:

    ```elixir
    def application do
      [applications: [:mixcord]]
    end
    ```

