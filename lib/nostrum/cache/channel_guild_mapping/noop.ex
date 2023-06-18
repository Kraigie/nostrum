defmodule Nostrum.Cache.ChannelGuildMapping.NoOp do
  @moduledoc """
  NoOp implementation for the Channel Guild map
  """
  @moduledoc since: "0.9.0"

  alias Nostrum.Cache.ChannelGuildMapping

  @behaviour ChannelGuildMapping

  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl Supervisor
  def init(_init_arg) do
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl ChannelGuildMapping
  def create(_channel_id, _guild_id), do: true

  @impl ChannelGuildMapping
  def get(_channel_id), do: nil

  @impl ChannelGuildMapping
  def delete(_channel_id), do: true
end
