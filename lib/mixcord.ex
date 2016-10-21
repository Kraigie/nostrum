defmodule Mixcord do
  @moduledoc """
  How to use this lib

  ## API
  Api functionality can be found in the `Mixcord.Api` module. Ratelimit is handled
  by the lib.

  ### A note about Strings and Ints
  Currently, anything dealing with the REST api will have its `id` as a string.
  This includes sending payloads as well as the returned payloads.

  Everything received from the WS connection will have and int `id`. You should convert
  id's to ints when interacting with any of the Caches. I'm open to suggestions
  for how this should be handled going forward.

  ## State
  Mixcord provides a number of GenServers which interface with ETS tables.
  These tables are updated based on events from the WS connection.

  The ETS tables are named so you can interact with them using any default ETS methods.
  To find tables names, please see the relevant docs for that cache.
  *Read operations should be the only operations performed on the ETS tables by the user.*

  See any of the Cache modules for default methods provided by the lib.

  ## Events
  Mixcord currently handles events to keep track of state. To implement your own
  behaviour you should define at least one method called `handle_event/2`. See the
  `Mixcord.Shard.Dispatch` module for more information.
  """

  use Application

  def start(_, _) do
    import Supervisor.Spec

    token = Application.get_env(:mixcord, :token)
    caller = Application.get_env(:mixcord, :caller)
    num_shards = Application.get_env(:mixcord, :num_shards)

    if !token, do: raise "Please supply a token"
    if !caller, do: raise "Please supply a caller"
    num_shards = if !num_shards, do: 1, else: num_shards

    setup_ets_tables

    children = [
      worker(Mixcord.Api.Ratelimiter, []),
      supervisor(Mixcord.Cache.Supervisor, []),
      supervisor(Mixcord.Shard.Supervisor, [token, caller, num_shards])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def setup_ets_tables do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    :ets.new(:gateway_url, [:set, :public, :named_table])
  end

  # allows us to run mixcord as a standalone
  def handle_event(_data, _state), do: :ok

end
