defmodule Nostrum.Store.RatelimitBucket do
  @default_implementation __MODULE__.ETS
  @moduledoc """
  Behaviour & dispatcher for storing ratelimit buckets.

  ## Purpose

  Calls to Discord's API are ratelimited, and we are informed about our
  remaining calls by Discord after issuing them. This module concerns itself
  with storing this information to prevent running into HTTP 429 errors. As
  this is used mainly by `Nostrum.Api.Ratelimiter`, it is unlikely you need to
  use it directly yourself.

  ## Configuration

  By default, nostrum will use `#{@default_implementation}` to store ratelimit
  buckets. To override this, set the `[:stores, :ratelimit_buckets]` setting on
  nostrum's application configuration:

  ```elixir
  config :nostrum,
    stores: %{
      ratelimit_buckets: MyBot.Nostrum.Store.RatelimitBucket
    }
  ```

  This setting must be set at compile time.

  ## Implementation

  If implementing your own ratelimit bucket store, in addition to the callbacks
  of this module, you must also provide the function `child_spec/1`. The
  recommended approach is to spawn a `Supervisor` to manage your store.
  """
  @moduledoc since: "0.8.0"

  @configured_store :nostrum
                    |> Application.compile_env(
                      [:stores, :ratelimit_buckets],
                      @default_implementation
                    )

  alias Nostrum.Util

  @typedoc """
  The route a bucket applies to.

  The constant value `"GLOBAL"` is used for the global bucket.
  """
  @type route :: String.t()

  @typedoc "Remaining calls for a bucket."
  @type remaining :: non_neg_integer()

  @typedoc "Time at which a bucket resets, in milliseconds."
  @type reset_time :: pos_integer()

  @typedoc "Latency between us and our latest call for this bucket, in milliseconds."
  @type latency :: pos_integer()

  @typedoc "Individual bucket information for a route."
  @type bucket :: {route(), remaining(), reset_time(), latency()}

  @doc """
  Update the given bucket to have the given remaining calls.

  Return whether the update has affected anything.

  Normally used after issuing an API call for a previously used route.
  """
  @callback update(route(), remaining()) :: boolean()

  @doc """
  Update the given bucket with full rate limit information.

  Whilst `c:update/2` is based around the assumption that you only know the
  remaining calls (for instance, after issuing one), this function is used
  after receiving full rate limiting information after an API call.
  """
  @callback update(route(), remaining(), reset_time(), latency()) :: true

  @doc """
  Retrieve bucket information by route.

  If no information is available, return `nil`.
  """
  @callback lookup(route()) :: bucket() | nil

  @doc """
  Clean up entries in the bucket older than the given amount of milliseconds.

  This function is called automatically by the ratelimiter in regular
  intervals.

  Return the amount of deleted entries.
  """
  @callback cleanup(pos_integer()) :: non_neg_integer()

  @doc """
  Retrieve the child specification for starting this mapping under a supervisor.
  """
  @callback child_spec(term()) :: Supervisor.child_spec()

  @doc """
  Receive the time to wait before issuing more calls to the given route.

  This function must only be called prior to issuing the actual API call: it
  will decrement the remaining calls counter from the given bucket.

  Nostrum takes the API latency into account when issuing these calls, and uses
  the current time to determine when it expires. It is therefore assumed that
  each node has a relatively equal latency to the API, and the nodes have
  little to no time drift.
  """
  @spec timeout_for(route()) :: remaining() | :now
  def timeout_for(route) do
    case lookup(route) do
      # XXX: In multi-node - or rather, with multiple rate limiter processes,
      # this poses a race condition. It needs to be atomic.
      {route, remaining, _reset_time, _latency} when remaining > 0 ->
        update(route, remaining - 1)
        :now

      {_route, _remaining, reset_time, latency} ->
        # Oh fuck. What the fuck did I just get myself into. I just realized:
        # if we introduce some shared storage for this, whatever it is, then we
        # probably also need to introduce some form of fucking time
        # synchronization or some other absolute hellspawn of evilkind into
        # this pure project. How would the latency be of value for the other
        # servers? Well, if it's the same data center or even the same city our
        # state, it's probably fine. But a unix timestamp? In milliseconds?
        # Distributed across servers? What the fuck, man. Do people configure
        # ntpd properly? Do people even know what ntpd is anymore? Do I even
        # know what I'm doing anymore?

        case reset_time - Util.now() + latency do
          time when time <= 0 -> :now
          time -> time
        end

      nil ->
        :now
    end
  end

  defdelegate update(route, remaining), to: @configured_store
  defdelegate update(route, remaining, reset_time, latency), to: @configured_store
  defdelegate lookup(route), to: @configured_store
  defdelegate cleanup(age), to: @configured_store
  @doc false
  defdelegate child_spec(opts), to: @configured_store
end
