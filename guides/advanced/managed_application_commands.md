# Managed Application Commands

While developing your own bot, it can be annoying to have to manually manage what commands you have registered with Discord, what permissions they have, and checking if they need to be synced. This guide will show you how to write your manager for application commands.

## Additional Dependencies
This guide requires the additional dependencies of
- [Ecto](https://hexdocs.pm/ecto/Ecto.html) for database interactions
- [EctoSQL](https://hexdocs.pm/ecto_sql/Ecto.Adapters.SQL.html)
- Your database adapter of choice (e.g. [Postgrex](https://hexdocs.pm/postgrex/Postgrex.html))

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
      timestamps()
    end
  end
end
```

## Writing the Command schema
Next, lets add a schema for the commands:

```elixir
defmodule MyApp.Commands.Command do
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
    field :creation_map, CommandMap
    timestamps()
  end
end
```

## Writing the Command Manager

We have a way to store the commands, the guild they belong to, the configuration we sent to Discord, and the time they were created. Now we need to actually manage
the commands.
