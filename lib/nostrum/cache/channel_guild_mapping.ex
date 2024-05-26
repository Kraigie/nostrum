defmodule Nostrum.Cache.ChannelGuildMapping do
  @default_implementation __MODULE__.ETS
  @moduledoc """
  Cache behaviour & dispatcher for mapping channel IDs to their respective guilds.

  ## Purpose

  While retrieving the channels on a specific guild is trivial as they are
  stored on the guild, retrieving the guild a given channel belongs to is not
  possible with the regular data mode of channels. This module allows to
  retrieve guilds associated with a channel via `get/1`.

  ## Configuration

  By default, nostrum will use `#{@default_implementation}` to store the
  mapping. To override this, set the `[:caches, :channel_guild_mapping]`
  setting on nostrum's application configuration:

  ```elixir
  config :nostrum,
    caches: %{
      channel_guild_mapping: MyBot.Nostrum.Cache.ChannelGuildMapping
    }
  ```

  This setting must be set at compile time.
  """
  @moduledoc since: "0.8.0"

  @configured_cache Nostrum.Cache.Base.get_cache_module(
                      :channel_guild_mapping,
                      @default_implementation
                    )

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild

  @doc """
  Create a mapping of the given channel to the given guild.
  """
  @callback create(Channel.id(), Guild.id()) :: true

  @doc """
  Retrieve the guild ID for the given channel ID, if present.
  """
  @callback get(Channel.id()) :: Guild.id() | nil

  @doc """
  Remove any mapping associated with the given channel.
  """
  @callback delete(Channel.id()) :: true

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  defdelegate create(guild_id, shard_num), to: @configured_cache
  defdelegate delete(guild_id), to: @configured_cache
  defdelegate get(guild_id), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache
end
