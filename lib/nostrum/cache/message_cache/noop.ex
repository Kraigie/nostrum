defmodule Nostrum.Cache.MessageCache.Noop do
  @moduledoc """
  A no-op message cache.

  This cache does not store any messages and always returns `{:error, :not_found}`
  for any operation.
  """
  @moduledoc since: "0.10.0"

  @behaviour Nostrum.Cache.MessageCache

  alias Nostrum.Cache.MessageCache
  alias Nostrum.Struct.Message
  alias Nostrum.Util

  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl Supervisor
  def init(_init_arg) do
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl MessageCache
  def get(_message_id), do: {:error, :not_found}

  @impl MessageCache
  def create(payload), do: Util.cast(payload, {:struct, Message})

  @impl MessageCache
  def update(payload), do: {nil, Util.cast(payload, {:struct, Message})}

  @impl MessageCache
  def delete(_channel_id, _message_id), do: nil

  @impl MessageCache
  def bulk_delete(_channel_id, _message_ids), do: []

  @impl MessageCache
  def channel_delete(_channel_id), do: :ok

  @impl MessageCache
  def get_by_author(_author, _after_timestamp, _before_timestamp), do: []

  @impl MessageCache
  def get_by_channel(_channel_id, _after_timestamp, _before_timestamp), do: []

  @impl MessageCache
  def get_by_channel_and_author(_channel_id, _author, _after_timestamp, _before_timestamp), do: []
end
