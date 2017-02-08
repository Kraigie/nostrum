defmodule Mixcord do
  @moduledoc """
  How to use Mixcord.

  ## API
  Api functionality can be found in the `Mixcord.Api` module. Ratelimiting is handled
  internally by the lib.

  ## Structs
  Mixcord uses structs when appropriate to pass around objects from Discord. In some
  instances, such as the cache and dispatch event handles, Discord's objects will be
  represented as maps. It will be made clear when this is the case using `@spec`
  specifications.

  In some cases, the struct modules will include helper functions for interacting
  with the struct. See `Mixcord.Struct.Emoji.format_custom_emoji/2` for an example.

  ## State/Cache
  Mixcord provides a number of GenServers to maintain state.
  These tables are updated based on events from the WS connection.

  See any of the Cache modules for default methods provided by the lib; e.g.
  `Guild` information can be found at `Mixcord.Cache.Guild`

  ## Event Handling
  Mixcord currently uses a GenStage implementation to handle dispatching events
  from the WS connection. See `Mixcord.Shard.Dispatch.Producer` for information
  on how to consume these events.
  """

  use Application

  @doc false
  def start(_, _) do
    import Supervisor.Spec

    token = Application.get_env(:mixcord, :token)
    caller = Application.get_env(:mixcord, :caller)
    num_shards = Application.get_env(:mixcord, :num_shards)

    if !token, do: raise "Please supply a token"
    if !caller, do: raise "Please supply a caller"
    actual_num_shards = if num_shards, do: num_shards, else: 1

    setup_ets_tables()

    children = [
      worker(Mixcord.Api.Ratelimiter, []),
      supervisor(Mixcord.Cache.Supervisor, []),
      supervisor(Mixcord.Shard.Supervisor, [token, caller, actual_num_shards])
    ]

    supervisor = Supervisor.start_link(children, strategy: :one_for_one)

    if Application.get_env(:mixcord, :dev, nil) do
      Dummy.start
    end

    supervisor
  end

  @doc false
  def setup_ets_tables do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    :ets.new(:gateway_url, [:set, :public, :named_table])
  end

end
