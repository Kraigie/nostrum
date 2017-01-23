# Mixcord

Mixcord is a wrapper for the Discord API made in [Elixir](http://elixir-lang.org/).

Mixcord currently supports the latest stable release of Elixir, v. 1.4

**No attempts will be made to support older versions of Elixir**. You can expect Mixcord to be updated beside Elixir for the forseeable future.

Mixcord is currently undergoing rapid development. Breaking changes should be expected to be made with every commit up until a stable version is released.
We'll be loosely following [semver](http://semver.org/) when a stable branch is released.

## Documentation
Indev documentation can be found [here](https://kraigie.github.io/mixcord/).

Stable documentation doesn't exist yet. :^)

## Installation

Mixcord is not currently available on Hex and will not be available until the requirements outlined [here](https://github.com/Kraigie/mixcord/projects/1) are complete.

The indev version of Mixcord can be installed as:

  1. Add `mixcord` to your list of dependencies in `mix.exs`:

    ```elixir
    def deps do
      [{:mixcord, git: "https://github.com/Kraigie/mixcord.git"}]
    end
    ```

  2. Ensure `mixcord` is started before your application:

    ```elixir
    def application do
      [applications: [:mixcord]]
    end
    ```

  3. Create/edit your `config.exs` file to include the following information:
    ```elixir
      config :mixcord,
        token: "YOUR_API_TOKEN_HERE",
        caller: Module.Where.You.Define.Your.Handlers, <- This will likely be changed to be macro oriented soonish
        num_shards: # of shards you want to use <- This will be changed very soon
    ```

To update your version of Mixcord simply run `mix deps.update mixcord`

## Example
A simple example bot can be found [here](https://github.com/Kraigie/mixbot)
