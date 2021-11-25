defmodule Nostrum.Cache.ChannelCache do
  @default_cache_implementation Nostrum.Cache.ChannelCache.ETS
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

  alias Nostrum.Struct.Channel

  # TODO: add :channels to cache config
  @configured_cache :nostrum
                    |> Application.compile_env(:caches, %{})
                    |> Map.get(:channels, @default_cache_implementation)

  @typedoc "Specifies the reason for why a lookup operation has failed."
  @type reason :: :channel_not_found

  ## Supervisor callbacks
  # These set up the backing cache.
  @doc false
  defdelegate init(init_arg), to: @configured_cache
  @doc false
  defdelegate start_link(init_arg), to: @configured_cache
  @doc false
  defdelegate child_spec(opts), to: @configured_cache

  ## Behaviour specification

  @callback get(Channel.id() | Nostrum.Struct.Message.t()) :: {:error, reason} | {:ok, Channel.t()} | {:error, reason}

  @callback get!(Channel.id() | Nostrum.Struct.Message.t()) :: no_return | Channel.t()

  @callback create(map) :: Channel.t()

  @callback update(Channel.t()) :: :noop | {Channel.t(), Channel.t()}

  @callback delete(Channel.id()) :: :noop | Channel.t()

  @callback lookup(Channel.id()) :: {:error, reason} | {:ok, map}

  # Dispatching logic

  defdelegate get(channel_id), to: @configured_cache
  defdelegate get!(channel_id), to: @configured_cache
  defdelegate create(map), to: @configured_cache
  defdelegate update(channel), to: @configured_cache
  defdelegate delete(channel_id), to: @configured_cache
  defdelegate lookup(channel_id), to: @configured_cache
end
