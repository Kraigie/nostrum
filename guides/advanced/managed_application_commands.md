# Managed Application Commands

While developing your own bot, it can be annoying to have to manually manage
what commands you have registered with Discord, what permissions they have,
and checking if they need to be re-synced.

This guide will show you how to write a manager for application commands.

## Additional Dependencies
This guide requires the additional dependencies of
- [Ecto](https://hexdocs.pm/ecto/Ecto.html) for database interactions
- [EctoSQL](https://hexdocs.pm/ecto_sql/Ecto.Adapters.SQL.html)
- Your database adapter of choice (e.g. [EctoSQLite3](https://hexdocs.pm/ecto_sqlite3/Ecto.Adapters.SQLite3.html))

As such a certain amount of knowledge of Ecto is assumed.

## Setting up the Database Migration
First, add and run a migration file with the following content:

```elixir
defmodule MyApp.Repo.Migrations.CreateCommands do
  use Ecto.Migration

  def change do
    create table(:commands, primary_key: false) do
      add :name, :string, primary_key: true
      add :guild_id, :string, primary_key: true
      add :creation_map, :binary, null: false # We'll be storing it as ETF
      add :command_id, :string, null: false
      timestamps()
    end
  end
end
```

## Writing the Command schema
Next, lets add a schema for the commands:

```elixir
defmodule MyApp.Schemas.Command do
  use Ecto.Schema

  defmodule CommandMap do
    @moduledoc """
    Custom type for storing the command map as an ETF blob
    """

    use Ecto.Type

    @type t :: map()

    def type, do: :binary

    def cast(map) when is_map(map), do: {:ok, map}
    def cast(_), do: :error

    def load(blob) when is_binary(blob), do: {:ok, :erlang.binary_to_term(blob)}

    def dump(map) when is_map(map), do: {:ok, :erlang.term_to_binary(map)}
    def dump(_), do: :error
  end

  @primary_key false
  schema "commands" do
    field :name, :string, primary_key: true
    field :guild_id, :string, primary_key: true
    field :command_id, :string
    field :creation_map, CommandMap
    timestamps()
  end
end
```

## Our first slash command
We have a way to store the commands, the guild they belong to,
the configuration we sent to Discord, but we don't yet have a
way to define the commands themselves.

So lets create a behaviour for that:

```elixir
defmodule MyApp.SlashCommand do
  @doc """
  Function returning a list of guild IDs the command should be registered in,
  or the atom `:global` if it should be registered globally.

  We call this function after startup to see if we need to register the command
  anywhere new or remove it from any guilds.
  """
  @callback guild_id() :: [String.t()] | :global

  @doc """
  Function that returns a map representing the command
  which will be sent to Discord as-is.

  NOTE: In this example we will be calling this function at
  compile time, so lets make sure it has little to no side effects.
  """
  @callback creation_map() :: map()

  @doc """
  Function that is actually called when the command is invoked.
  Return value is ignored.
  """
  @callback call(Nostrum.Struct.Interaction.t()) :: term()
end
```

Now we can define our first command:

```elixir
defmodule MyApp.Commands.Ping do
  @behaviour MyApp.SlashCommand

  @impl MyApp.SlashCommand
  def guild_id(), do: :global

  @impl MyApp.SlashCommand
  def creation_map() do
    %{
      type: 1,
      name: "ping",
      description: "Ping the bot"
    }
  end

  @impl MyApp.SlashCommand
  def call(interaction) do
    Nostrum.Api.Interaction.create_response(
      interaction,
      %{
        type: 4,
        data: %{
          content: "Pong!"
        }
      }
    )
  end
end
```

Lets add that to our config so that we can access it later:

```elixir
config :my_app, commands: [MyApp.Commands.Ping]
```


## Writing the Command Manager

We have a way to store the commands, the guild they belong to,
the configuration we sent to Discord, 
and the time they were created, and our first command.

Now we need to actually manage the commands.

```elixir
defmodule MyApp.CommandManager do
  import Ecto.Query, only: [from: 2]
  alias MyApp.Schemas.Command
  
  @doc """
  Iterate through all commands and register them with Discord.

  Expects `config :my_app, commands:` to have been set to a list of modules
  that implement the `MyApp.SlashCommand` behaviour.
  """
  def register_commands() do
    for command_module <- Application.get_env(:my_app, :commands) do
      parse_command_info(command_module)
    end
  end

  defp parse_commnd_info(command_module) do
    creation_map = command_module.creation_map()

    case command_module.guild_id() do
      :global ->
        maybe_register_command(creation_map, "GLOBAL")

      guild_ids when is_list(guild_ids) ->
        for guild_id <- guild_ids do
          maybe_register_command(creation_map, guild_id)
        end
    end
  end

  defp maybe_register_command(creation_map, guild_id) do
    command_name = creation_map.name

    command = from(
      c in Command,
      where: c.name == ^command_name and c.guild_id == ^guild_id
    ) |> MyApp.Repo.one()

    case command do
      nil ->
        register_command(creation_map, guild_id)

      %Command{creation_map: old_creation_map} when old_creation_map != creation_map ->
        # For discord, all command registrations are upserts
        register_command(creation_map, guild_id)

      _ ->
        :ok
    end
  end

  defp register_command(creation_map, "GLOBAL") do
    {:ok, %{id: id}} = Nostrum.Api.ApplicationCommand.create_global_command(creation_map)

    %Command{
      name: creation_map.name,
      guild_id: "GLOBAL",
      command_id: id,
      creation_map: creation_map
    } |> MyApp.Repo.insert!(
      on_conflict: {:replace, [:command_id, :creation_map]},
    )
  end

  def register_command(creation_map, guild_id) do
    {:ok, %{id: id}} = Nostrum.Api.ApplicationCommand.create_guild_command(guild_id, creation_map)

    %Command{
      name: creation_map.name,
      guild_id: guild_id,
      command_id: id,
      creation_map: creation_map
    } |> MyApp.Repo.insert!(
      on_conflict: {:replace, [:command_id, :creation_map]},
    )
  end
end
```

We've now got a command manager, but where should we call it?
That's up to you, but a good place might be in your event handler on
the `:READY` event.

## Dispatch

So after all that work we still need a way to actually dispatch the commands.
Lets handle that at compile time with a little macro magic:

```elixir
defmodule MyApp.CommandDispatch do
  alias Nostrum.Api
  alias Nostrum.Struct.Interaction

  require Logger

  @spec dispatch(Interaction.t()) :: term()
  def dispatch(%Interaction{} = inter) do
    handle_command(inter.data.name, inter)
  end

  @commands Application.get_env(:my_app, :commands)

  for cmd <- @commands do
    Code.ensure_compiled!(cmd)

    name = cmd.get_create_map()[:name]
    true = function_exported?(cmd, :call, 1)

    defp handle_slash_command(unquote(name), inter) do
      unquote(cmd).call(inter)
    end
  end

  # add in a base case for when we have unrecognized commands
  defp handle_slash_command(name, inter) do
    Logger.warning("Unrecognized command: #{name}")
    Api.Interaction.create_response(
      inter,
      %{
        type: 4,
        data: %{
          content: "Unrecognized command: #{name}",
          flags: 64
        }
      }
    )
  end
end
```

So what's with the loop comprehension at compile time and
all the `unquote` calls?

Well, we're using the `@commands` module attribute to store
the list of commands we want to dispatch. We then loop over
each command, ensure its already compiled, and check if it
has a `call/1` function. If it does, we define a new
clause of `handle_slash_command/2` for that particular command.

In the case of a `ping` command, the generated code would look like:

```elixir
defp handle_slash_command("ping", inter) do
  MyApp.Commands.Ping.call(inter)
end
```

If we have a lot of commands, this means that whenever we want to add a new command
we only need to update a config value then recompile and we're done.

Managing commands being removed and multibot support is left as an exercise for the reader.
