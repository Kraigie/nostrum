defmodule Nostrum.Cache.MessageCache.Noop do
  @moduledoc """
  A no-op message cache.

  This cache does not store any messages and always returns `{:error, :not_found}`
  for any operation.
  """

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
  def delete(_channel_id, _message_id), do: :noop

  @impl MessageCache
  def bulk_delete(channel_id, message_ids) do
    Enum.map(message_ids, fn message_id ->
      Message.to_struct(%{id: message_id, channel_id: channel_id})
    end)
  end

  @impl MessageCache
  def channel_delete(_channel_id), do: :ok

  @impl Nostrum.Cache.MessageCache
  def query_handle, do: :qlc.string_to_handle(~c"[].")
end
