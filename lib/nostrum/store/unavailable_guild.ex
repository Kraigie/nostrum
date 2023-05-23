defmodule Nostrum.Store.UnavailableGuild do
  @default_implementation __MODULE__.ETS
  @moduledoc """
  Behaviour & dispatcher for storing unavailable guilds.

  ## Purpose

  The `GUILD_CREATE` gateway event on its own provides no means to determine
  whether the guild we receive is a guild that the bot joined, or a guild that
  has just become available over the gateway. To work around this, this store
  keeps track of unavailable guilds we received to determine whether a guild
  sent over this event is unavailable or new. It is therefore unlikely this
  module needs to be used outside of nostrum.

  ## Configuration

  By default, nostrum will use `#{@default_implementation}` to store
  unavailable guilds. To override this, set the `[:stores,
  :unavailable_guilds]` setting on nostrum's application configuration:

  ```elixir
  config :nostrum,
    stores: %{
      unavailable_guilds: MyBot.Nostrum.Store.UnavailableGuild
    }
  ```

  This setting must be set at compile time.

  ## Implementation

  If implementing your own unavailable guild store, in addition to the
  callbacks of this module, you must also provide the function `child_spec/1`.
  The recommended approach is to spawn a `Supervisor` to manage your store.
  """
  @moduledoc since: "0.8.0"

  alias Nostrum.Struct.Guild

  @configured_store :nostrum
                    |> Application.compile_env(
                      [:stores, :unavailable_guilds],
                      @default_implementation
                    )

  @doc """
  Mark the given guild as unavailable.
  """
  @callback create(Guild.id()) :: :ok

  @doc """
  Return whether the guild is unavailable.
  """
  @callback is?(Guild.id()) :: boolean()

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  defdelegate create(guild_id), to: @configured_store
  defdelegate is?(guild_id), to: @configured_store

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate child_spec(opts), to: @configured_store
end
