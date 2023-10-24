defmodule Nostrum.Cache.ChannelCache.NoOp do
  @moduledoc """
  A NoOp implementation for the ChannelCache

  This cache does nothing, enable it if you dont need to cache channels
  """
  @moduledoc since: "0.9.0"

  @behaviour Nostrum.Cache.ChannelCache

  alias Nostrum.Cache.ChannelCache
  alias Nostrum.Struct.Channel
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl Supervisor
  def init(_init_arg) do
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl ChannelCache
  def create(channel), do: convert(channel)

  @impl ChannelCache
  def update(channel), do: {nil, convert(channel)}

  @impl ChannelCache
  def delete(_id), do: :noop

  @impl ChannelCache
  def query_handle, do: :qlc.string_to_handle(~c"[].")

  defp convert(%{__struct__: _} = struct), do: struct
  defp convert(map), do: Channel.to_struct(map)
end
