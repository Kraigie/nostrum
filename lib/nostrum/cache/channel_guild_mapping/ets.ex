defmodule Nostrum.Cache.ChannelGuildMapping.ETS do
  @table_name :nostrum_channel_guild_mapping
  @moduledoc """
  Maps channels to guilds via `:ets`.

  Please use the function `table/0` for retrieving a reference to the table
  used by nostrum.
  """
  @moduledoc since: "0.8.0"

  alias Nostrum.Cache.ChannelGuildMapping
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild

  @behaviour ChannelGuildMapping

  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Set up the ETS table."
  @impl Supervisor
  def init(_init_arg) do
    _tid = :ets.new(@table_name, [:set, :public, :named_table])
    Supervisor.init([], strategy: :one_for_one)
  end

  @doc "Retrieve the table used by this module."
  @spec table() :: :ets.table()
  def table, do: @table_name

  @impl ChannelGuildMapping
  @doc "Create a mapping of the given channel to the given guild."
  @spec create(Channel.id(), Guild.id()) :: true
  def create(channel_id, guild_id) do
    :ets.insert(@table_name, {channel_id, guild_id})
  end

  @impl ChannelGuildMapping
  @doc "Retrieve the guild ID for the given channel ID, if present."
  @spec get(Channel.id()) :: Guild.id() | nil
  def get(channel_id) do
    case :ets.lookup(@table_name, channel_id) do
      [{_channel, guild_id}] -> guild_id
      [] -> nil
    end
  end

  @impl ChannelGuildMapping
  @doc "Remove any mapping associated with the given channel."
  @spec delete(Channel.id()) :: true
  def delete(channel_id) do
    :ets.delete(@table_name, channel_id)
  end
end
