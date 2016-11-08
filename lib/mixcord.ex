defmodule Mixcord do
  @moduledoc """
  How to use Mixcord.

  ## API
  Api functionality can be found in the `Mixcord.Api` module. Ratelimiting is handled
  by the lib.

  ### A note about Strings and Ints
  Currently, responses from the REST api will have `id` fields as `string`.
  Everything received from the WS connection will have `id` fields as `int`.

  If you're processing a response from the API and trying to access something in the cache
  based off of an `id` in the response, you will need to conver it to an `int` using
  `String.to_integer/1`. I'm open to suggestions for how this should be handled going forward.

  ## State/Cache
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
