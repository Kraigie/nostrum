defmodule Nostrum.Store.GuildShardMapping do
  @default_implementation __MODULE__.ETS
  @moduledoc """
  Behaviour & dispatcher for storing guild to shard mappings.

  ## Purpose

  When nostrum needs to run API calls over the gateway and multiple shards are
  used, it needs to determine which shard process will run the request. This
  module stores a mapping of guild IDs to their respective shard numbers. It is
  unlikely you need to use this module directly yourself.

  ## Configuration

  By default, nostrum will use `#{@default_implementation}` to store the
  mapping. To override this, set the `[:stores, :guild_shard_mapping]` setting
  on nostrum's application configuration:

  ```elixir
  config :nostrum,
    stores: %{
      guild_shard_mapping: MyBot.Nostrum.Store.GuildShardMapping
    }
  ```

  This setting must be set at compile time.
  """

  # XXX: This module should really be storing the Shard.Session PID instead, as
  # it currently relies on name registration of the shard, see
  # `Nostrum.Shard.Supervisor.update_voice_state`. For that to work, the
  # `WSState` struct needs to be expanded to store the shard PID in addition to
  # the shard number. It looks like this is already done using the `conn_pid`
  # field. However, this introduces an additional problem: when the shard
  # session goes down, we need to update this module with the new shard PID
  # associated with the guild.
  #
  # The following solution is proposed: - Keep the mapping of guild_id ->
  # shard_num - Introduce a mapping of shard_num -> shard_pid => :global Shard
  # sessions register themselves via `:global`
  #
  # So we need to add another new mapping. Or actually: we don't, because we
  # will basically reinvent `:global` at that point, so let's use `:global`.

  @configured_store :nostrum
                    |> Application.compile_env(
                      [:stores, :guild_shard_mapping],
                      @default_implementation
                    )

  @moduledoc since: "0.8.0"

  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.WSState

  @doc """
  Create a new mapping for the given guild ID to the given shard ID.
  """
  @callback create(Guild.id(), WSState.shard_num()) :: :ok

  @doc """
  Delete any stored mapping for the given guild ID.
  """
  @callback delete(Guild.id()) :: :ok

  @doc """
  Retrieve the shard number for the given guild ID.

  Returns `nil` if no associated shard number was found.
  """
  @callback get(Guild.id()) :: WSState.shard_num() | nil

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  defdelegate create(guild_id, shard_num), to: @configured_store
  defdelegate delete(guild_id), to: @configured_store
  defdelegate get(guild_id), to: @configured_store
  @doc false
  defdelegate child_spec(opts), to: @configured_store
end
