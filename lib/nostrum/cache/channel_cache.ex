defmodule Nostrum.Cache.ChannelCache do
  @moduledoc """
  Cache for channels.

  The ETS table name associated with the Channel Cache is `:channels`. Besides the
  methods provided below you can call any other ETS methods on the table.

  ## Example
  ```elixir
  info = :ets.info(:channels)
  [..., heir: :none, name: :channels, size: 1, ...]
  size = info[:size]
  1
  ```
  """

  alias Nostrum.Cache.GuildCache
  alias Nostrum.Struct.Channel
  alias Nostrum.Util

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc ~S"""
  Retrieves a channel from the cache.

  Internally, the ChannelCache process only stores
  `t:Nostrum.Struct.Channel.dm_channel/0` references. To get channel
  information, a call is made to a `Nostrum.Cache.GuildCache`.

  If successful, returns `{:ok, channel}`. Otherwise, returns `{:error, reason}`

  ## Example
  ```elixir
  case Nostrum.Cache.ChannelCache.get(133333333337) do
    {:ok, channel} ->
      "We found " <> channel.name
    {:error, _reason} ->
      "Donde esta"
  end
  ```
  """
  @spec get(Channel.id() | Nostrum.Struct.Message.t()) :: {:error, atom} | {:ok, Channel.t()}
  def get(%Nostrum.Struct.Message{channel_id: channel_id}), do: get(channel_id)

  def get(id) when is_snowflake(id) do
    case lookup(id) do
      {:ok, channel} -> {:ok, convert(channel)}
      error -> error
    end
  end

  @doc ~S"""
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of a failure.
  """
  @spec get!(Channel.id() | Nostrum.Struct.Message.t()) :: no_return | Channel.t()
  def get!(%Nostrum.Struct.Message{channel_id: channel_id}), do: get!(channel_id)
  def get!(id) when is_snowflake(id), do: id |> get |> Util.bangify_find(id, __MODULE__)

  @doc false
  @spec create(map) :: Channel.t()
  def create(channel) do
    :ets.insert(:channels, {channel.id, channel})
    convert(channel)
  end

  @doc false
  @spec update(Channel.t()) :: :noop | {Channel.t(), Channel.t()}
  def update(channel) do
    case lookup(channel.id) do
      {:ok, old_channel} ->
        {convert(old_channel), convert(channel)}

      _ ->
        :noop
    end
  end

  @doc false
  @spec delete(Channel.id()) :: :noop | Channel.t()
  def delete(id) do
    case lookup(id) do
      {:ok, channel} ->
        :ets.delete(:channels, id)
        convert(channel)

      _ ->
        :noop
    end
  end

  @doc false
  @spec lookup(Channel.id()) :: {:error, :channel_not_found} | {:ok, map}
  def lookup(id) do
    case :ets.lookup(:channels, id) do
      [] ->
        [channel_id: id]
        |> GuildCache.select_by(fn %{channels: channels} ->
          Map.get(channels, id, {:error, :id_not_found})
        end)
        |> case do
          {:error, :id_not_found} ->
            {:error, :channel_not_found}
          res ->
            res
        end

      [{^id, channel}] ->
        {:ok, channel}
    end
  end

  @doc false
  def convert(%{__struct__: _} = struct), do: struct
  def convert(map), do: Channel.to_struct(map)
end
