defmodule Nostrum.Api.Ratelimiter do
  @moduledoc """
  Handles REST calls to the Discord API while respecting ratelimits.


  ## Purpose

  Discord's API returns information about ratelimits that we must respect. This
  module performs serialization of these requests through a single process,
  thus preventing concurrency issues from arising if two processes make a
  remote API call at the same time.


  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and people curious to look behind the covers.


  ## Asynchronous requests

  The ratelimiter is fully asynchronous internally. In theory, it also supports
  queueing requests in an asynchronous manner. However, support for this is
  currently not implemented in `Nostrum.Api`.

  If you want to make one or multiple asynchronous requests manually, you can
  use the following pattern:

  ```elixir
  req = :gen_statem.send_request(Nostrum.Api.Ratelimiter, {:queue, request})
  # ...
  response = :gen_statem.receive_response(req, timeout)
  ```

  where `request` is a map describing the request to run - see `Nostrum.Api`
  for more information. You can also send multiple requests at the same time
  and wait for their response: see `:gen_statem.reqids_add/3` and
  `:gen_statem.wait_response/3` for more information.


  ## Multi-node

  nostrum will transparently distribute client requests across all ratelimiter
  clusters running in the cluster. This allows us to account for per-route
  ratelimits whilst still distributing work across cluster nodes. **Note that
  the API enforces a global user ratelimit across all requests**, which we
  cannot account for using this method.


  ## Inner workings

  When a client process wants to perform some request on the Discord API, it
  sends a request to the `:gen_statem` behind this module to ask it to `:queue`
  the incoming request.


  ### Connection setup

  If the state machine is not connected to the HTTP endpoint, it will
  transition to the `:connecting` state and try to open the connection. If this
  succeeds, it transitions to the `:connected` state.

  ### Queueing requests

  The state machine associates a `t::queue.queue/1` of `t:queued_request/0` to
  each individual bucket, together with an internal count of remaining calls.
  When queueing requests, the following cases occur:

  - If there are no remaining calls in the bot's global ratelimit bucket or
  there are no remaining calls in the bucket, the request is put into the
  bucket's queue.

  - If there is an `:initial` running request to the bucket, the request is put
  into the bucket's queue.

  - If there are more than 0 remaining calls on both the request-specific
  bucket and the global bucket, the request is started right away. This allows
  nostrum to dispatch multiple requests to the same endpoint as soon as
  possible as long as calls remain.

  - If no ratelimit information is known for the bucket and remaining calls on
  the global bucket, the request is sent out as the "pioneer" request that will
  retrieve how many calls we have for this bucket (`:initial`, see above).

  - If none of the above is true, a new queue is created and the pending
  rqeuest marked as the `:initial` request. It will be run as soon as the bot's
  global limit limit expires.

  The request starting function, `:next`, will start new requests from the
  queue as long as more calls are possible in the timeframe. Any requests are
  then started asynchronously. Bookkeeping is set up to associate the resulting
  `t::gun.stream_ref/0` with the original client along with its request and the
  ratelimiter bucket.

  Results from the HTTP connection are delivered non-blocking: simple responses
  with purely status codes and no body (code `204`) will be sent in a single
  message, other requests will be sent to us incrementally. To finally deliver
  the full response body to the client with the final package, an internal
  buffer of the body is kept. A possible future optimization could be having a
  way for `:gun` to only send the ratelimiter state machine the initial
  `:gun_response` and forward any item of the body directly to the client.

  When the headers for a request have been received, the ratelimiter parses the
  ratelimit information and starts off an internal timer expiring when the
  ratelimits expire. It will also reschedule calls with the `:next` internal
  event for as many remaining calls as it knows about. Once the timer expires
  for the current bucket, two cases can happen:

  - The queue has items: Schedule all items and repeat this later.

  - The queue is empty: Delete the queue and remaining calls from the
  outstanding buckets.

  In practice, this means that we never store more information than we need,
  and removes the previous regular bucket sweeping functionality that the
  ratelimit buckets required.

  **Global ratelimits** (note this is a distinct ratelimit from the bot's
  "global", per-user ratelimit) are handled with the special `global_limit`
  state. This state is entered for exactly the the `X-Ratelimit-Reset-After`
  time provided in the global ratelimit response. This state does nothing apart
  from postponing any events it receives and returning to the previous state
  (`:connected`) once the global timeout is gone. Requests that failed because
  of the global ratelimit are requeued after returning back into the regular
  state: a warning is logged to inform you of this.


  ### Failure modes

  #### HTTP connection death

  If the HTTP connection dies, the ratelimiter will inform each affected client
  by replying with `{:error, {:connection_died, reason}}`, where `reason` is
  the reason as provided by the `:gun_down` event. It will then transition to
  `:disconnected` state. If no requests were running at time the connection was
  shut down - for instance, because we simply reached the maximum idle time on
  the HTTP/2 connection - we will simply move on.

  #### Upstream errors

  The ratelimiter works by queueing requests aggressively as soon as it has
  ratelimit information to do so. If no ratelimit information is available, for
  instance, because Discord returned us a 502 status code, the ratelimiter will
  not automatically kick the queue to start further running requests.

  #### Other internal issues

  Any other internal problems that are not handled appropriately in the
  ratelimiter will crash it, effectively resulting in the complete loss of any
  queued requests.


  ### Implementation benefits & drawbacks

  #### A history of ratelimiting

  First, it is important to give a short history of nostrum's ratelimiting: pre
  `0.8`, nostrum used to use a `GenServer` that would call out to ETS tables to
  look up ratelimiting buckets for requests. If it needed to sleep before
  issuing a request due to the bucket being exhausted, it would do so in the
  server process and block other callers.

  In nostrum 0.8, the existing ratelimiter bucket storage architecture was
  refactored to be based around the [pluggable caching
  functionality](../advanced/pluggable_caching.md), and buckets with no
  remaining calls were adjusted to be slept out on the client-side by having
  the `GenServer` respond to the client with `{:error, {:retry_after, millis}}`
  and the client trying again and again to schedule its requests. This allowed
  users to distribute their ratelimit buckets around however they wish, out of
  the box, nostrum shipped with an ETS and a Mnesia-based ratelimit bucket
  store.


  #### Problems we solved

  The approach above still came with a few problems:

  - Requests were still being done synchronously in the ratelimiter, and it was
  blocked from anything else whilst running the requests, even though we are
  theoretically free to start requests for other buckets while one is still
  running.

  - The ratelimiter itself was half working on its own, but half required the
  external storage mechanisms, which made the code hard to follow and required
  regular automatic pruning because the store had no idea when a bucket was no
  longer relevant on its own.

  - Requests would not be pipelined to run as soon as ideally possible.

  - The ratelimiter did not inform clients if their request died in-flight.

  - If the client disconnected before we returned the response, we had to
  handle this explicitly via `handle_info`.

  The new state machine-based ratelimiter solves these problems.
  """

  @behaviour :gen_statem

  alias Nostrum.Api.Adapter
  alias Nostrum.Api.RatelimiterGroup
  alias Nostrum.Constants
  alias Nostrum.Error.ApiError

  require Logger

  @major_parameters ["channels", "guilds", "webhooks"]
  @registered_name __MODULE__
  # Discord mandates a specific number of API calls we may make across (almost)
  # all endpoints per second. See the "global rate limit" documentation for
  # more information:
  # https://discord.com/developers/docs/topics/rate-limits#global-rate-limit
  @bot_calls_per_window 50
  @bot_calls_time_window :timer.seconds(1)
  @bot_calls_timeout_event :reset_bot_calls_window

  # Retry requests for buckets with no known ratelimit information that were
  # abormally closed after this much time.
  @retry_abnormal_close_after :timer.seconds(1)
  # Retry requests for buckets with no known ratelimit information that hit a
  # 429 after this much time.
  @retry_429s_after :timer.seconds(10)

  @typedoc """
  A bucket for endpoints unter the same ratelimit.
  """
  @typedoc since: "0.9.0"
  @type bucket :: String.t()

  @typedoc """
  A request to make in the ratelimiter.
  """
  @typedoc since: "0.9.0"
  @type request :: %{
          method: :get | :post | :put | :delete,
          route: String.t(),
          body: iodata(),
          headers: [{String.t(), String.t()}],
          params: Enum.t()
        }

  @typedoc """
  A bucket-specific request waiting to be queued, alongside its client.
  """
  @typedoc since: "0.9.0"
  @type queued_request :: {request(), client :: :gen_statem.from()}

  @typedoc """
  Remaining calls on a route, as provided by the API response.

  The ratelimiter internally counts the remaining calls per route to dispatch
  new requests as soon as it's capable of doing so, but this is only possible
  if the API already provided us with ratelimit information for an endpoint.

  Therefore, if the initial call on an endpoint is made, the special `:initial`
  value is specified. This is used by the limit parsing function to set the
  remaining calls if and only if it is the response for the initial call -
  otherwise, the value won't represent the truth anymore.
  """
  @typedoc since: "0.9.0"
  @type remaining :: non_neg_integer() | :initial

  @typedoc """
  The state of the ratelimiter.

  While this has no public use, it is still documented here to provide help
  when tracing the ratelimiter via `:sys.trace/2` or other means.

  ## Fields

  - `:outstanding`: Outstanding (unqueued) requests per bucket alongside with
  the remaining calls that may be made on said bucket.

  - `:running`: Requests that have been sent off. Used to associate back the
  client with a request when the response comes in.

  - `:inflight`: Requests for which we have started getting a response, but we
  have not fully received it yet. For responses that have a body, this will
  buffer their body until we can send it back to the client.

  - `:conn`: The `:gun` connection backing the server. Used for making new
  requests, and updated as the state changes.

  - `:remaining_in_window`: How many calls we may still make to the API during
  this time window. Reset automatically via timeouts.

  - `:wrapped_token`: An anonymous function that is internally used to retrieve
  the token. This is wrapped to ensure that it is not accidentally exposed in
  stacktraces.
  """
  @typedoc since: "0.9.0"
  @type state :: %{
          outstanding: %{
            bucket => {remaining, :queue.queue(queued_request)}
          },
          running: %{
            :gun.stream_ref() => {bucket(), request(), :gen_statem.from()}
          },
          inflight: %{
            :gun.stream_ref() =>
              {status :: non_neg_integer(), headers :: [{String.t(), String.t()}],
               body :: String.t()}
          },
          conn: pid() | nil,
          remaining_in_window: non_neg_integer(),
          wrapped_token: Adapter.wrapped_token()
        }

  @doc """
  Starts the ratelimiter.
  """
  @spec start_link({String.t(), [:gen_statem.start_opt()]}) :: :gen_statem.start_ret()
  def start_link({token, opts}) do
    :gen_statem.start_link({:local, @registered_name}, __MODULE__, token, opts)
  end

  def init(token) when is_binary(token) do
    :ok = RatelimiterGroup.join(self())
    # Uncomment the following to trace everything the ratelimiter is doing:
    #   me = self()
    #   spawn(fn -> :sys.trace(me, true) end)
    # See more examples in the `sys` docs.
    {:ok, :disconnected, empty_state(token)}
  end

  def callback_mode, do: :state_functions

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end

  # The Glorious State Machine
  # Inspired by Peter Morgan's "Postpone: Resource Allocation on Demand"
  #   https://shortishly.com/blog/postpone-resource-allocation-on-demand/

  def disconnected({:call, _from}, _request, data) do
    {:next_state, :connecting, data,
     [
       {:next_event, :internal, :open},
       {:state_timeout, :timer.seconds(10), :connect_timeout},
       :postpone
     ]}
  end

  # We were informed that a bucket expired, and we have outstanding requests
  # (that were previously ratelimited) for that bucket. We need to connect and
  # run it - just like when receiving a queue request above.
  def disconnected({:timeout, bucket}, :expired, %{outstanding: outstanding} = data)
      when is_map_key(outstanding, bucket) do
    {:next_state, :connecting, data,
     [
       {:next_event, :internal, :open},
       {:state_timeout, :timer.seconds(10), :connect_timeout},
       :postpone
     ]}
  end

  # We received a timeout for a bucket that does not have any pending requests.
  # This means that the remaining requests got to exactly 0 before we ceased
  # sending further requests.
  def disconnected({:timeout, _bucket}, :expired, _data) do
    :keep_state_and_data
  end

  def connecting(:internal, :open, data) do
    domain = to_charlist(Constants.domain())

    open_opts = get_open_opts()

    {:ok, conn_pid} = :gun.open(domain, 443, open_opts)
    {:keep_state, %{data | conn: conn_pid}}
  end

  def connecting(:info, {:gun_up, conn_pid, _}, %{conn: conn_pid} = data) do
    {:next_state, :connected, data}
  end

  def connecting({:call, _from}, _request, _data) do
    {:keep_state_and_data, :postpone}
  end

  def connecting({:timeout, _bucket}, :expired, _data) do
    {:keep_state_and_data, :postpone}
  end

  def connecting(:state_timeout, :connect_timeout, _data) do
    {:stop, :connect_timeout}
  end

  # Client request: Queue the given request, and respond when we have the response.
  def connected({:call, from}, {:queue, request}, _data) do
    {:keep_state_and_data, {:next_event, :internal, {:queue, {request, from}}}}
  end

  # Enqueue the passed request.
  def connected(
        :internal,
        {:queue, {payload, from}},
        %{outstanding: outstanding, remaining_in_window: remaining_in_window} = data
      ) do
    bucket = get_endpoint(payload.route, payload.method)

    # The outstanding maps contains pairs in the form `{remaining, queue}`,
    # where `remaining` is the amount of remaining calls we may make, and
    # `queue` is the waiting line of requests. If the ratelimit on the bucket
    # expires, the internal timeout event will automatically reschedule queued
    # requests (starting with a single one to get the calls we may make).
    case Map.get(outstanding, bucket) do
      # We have no remaining calls on the bucket, on the bot user, or the
      # initial call to get rate limiting information is in flight. Let's join
      # the waiting line.
      {remaining, queue} when remaining in [0, :initial] or remaining_in_window == 0 ->
        entry = {payload, from}

        data_with_this_queued =
          put_in(data, [:outstanding, bucket], {remaining, :queue.in(entry, queue)})

        {:keep_state, data_with_this_queued}

      # There is an entry - so somebody did find some ratelimiting information
      # here recently - but that entry tells us we may make a call right away.
      {remaining, queue} when remaining > 0 and remaining_in_window > 0 ->
        # Sanity check. This can be removed after release is considered stable.
        # Why should this be empty?
        # Because when we receive ratelimit information and see that there are
        # still items in the queue, we should internally schedule them right away.
        # Otherwise, we are mixing up the order.
        true = :queue.is_empty(queue)
        {:keep_state_and_data, [{:next_event, :internal, {:run, payload, bucket, from}}]}

      # There is no entry. We are the pioneer for this bucket...
      nil when remaining_in_window > 0 ->
        # ... and we are good on the bot ratelimits!

        # Since we don't have any explicit ratelimiting information for this
        # bucket yet, we set the remaining calls to the special `:initial`
        # value. The ratelimit response header parser uses this value to know
        # when it should update ratelimit information from upstream, and new
        # incoming requests will be held off appropriately.
        run_request = {:next_event, :internal, {:run, payload, bucket, from}}
        data_with_new_queue = put_in(data, [:outstanding, bucket], {:initial, :queue.new()})
        {:keep_state, data_with_new_queue, [run_request]}

      nil ->
        # ... but we are not good on the bot ratelimits! Add this to the queue.
        entry = {payload, from}

        queue = :queue.new()

        data_with_this_queued =
          put_in(data, [:outstanding, bucket], {:initial, :queue.in(entry, queue)})

        {:keep_state, data_with_this_queued}
    end
  end

  def connected(:internal, {:requeue, {request, from} = statem_request, reason}, %{
        outstanding: outstanding
      }) do
    bucket = get_endpoint(request.route, request.method)

    expirers =
      case outstanding do
        %{^bucket => {:initial, _queue}} ->
          # If we're heading here, that means that the request we wish to requeue
          # is likely (but not certain) the initial request to an endpoint.
          # The `:requeue` internal event is used to ask the ratelimiter to requeue
          # requests that have failed either 1. due to a 429, or 2. due to an error
          # with the stream, this function matches only the second case. Since
          # this is the initial request, the `:queue` logic will not send out
          # new requests and append them to the end of the queue, which would
          # cause the request (and any further going to the endpoint) to hang
          # indefinitely.

          requeue_after = requeue_after_for_reason(reason)

          Logger.warning(
            "Retrying request for reason #{reason} with no known ratelimit information to #{inspect(request.route)} queued by #{inspect(from)} in #{requeue_after}ms"
          )

          [{{:timeout, bucket}, requeue_after, :expired}]

        _ ->
          []
      end

    {:keep_state_and_data, [{:next_event, :internal, {:queue, statem_request}} | expirers]}
  end

  def connected(:internal, {:requeue, {_request, _from} = statem_request, _reason}, _data) do
    {:keep_state_and_data, {:next_event, :internal, {:queue, statem_request}}}
  end

  # Run the given request right now, and do any bookkeeping. We assert we are
  # good on remaining calls, so we can just run the request.
  def connected(
        :internal,
        {:run, request, bucket, from},
        %{
          conn: conn,
          outstanding: outstanding,
          remaining_in_window: remaining_for_user,
          wrapped_token: wrapped_token
        } = data
      )
      when remaining_for_user > 0 and is_map_key(outstanding, bucket) do
    stream =
      Adapter.request(
        conn,
        request.method,
        request.route,
        request.body,
        request.headers,
        request.params,
        wrapped_token
      )

    data_with_this_running = put_in(data, [:running, stream], {bucket, request, from})
    {:keep_state, data_with_this_running, {:next_event, :internal, {:account_request, bucket}}}
  end

  # Account for the request on the given bucket. At a full user (global)
  # bucket, start the timer to empty it after the timeout. Regardless of the
  # user bucket, account for the individual request.
  def connected(
        :internal,
        {:account_request, _bucket} = request,
        %{remaining_in_window: @bot_calls_per_window} = data
      ) do
    {:keep_state, %{data | remaining_in_window: @bot_calls_per_window - 1},
     [
       {{:timeout, @bot_calls_timeout_event}, @bot_calls_time_window, :expired},
       {:next_event, :internal, request}
     ]}
  end

  def connected(
        :internal,
        {:account_request, bucket},
        %{remaining_in_window: remaining_in_window, outstanding: outstanding} = data
      )
      when remaining_in_window > 0 do
    %{^bucket => entry} = outstanding

    case entry do
      {:initial, _queue} ->
        # This was the first request here, we just need to account the use requests.
        {:keep_state, %{data | remaining_in_window: remaining_in_window - 1}}

      {remaining_in_bucket, queue} ->
        {:keep_state,
         %{
           data
           | outstanding: %{outstanding | bucket => {remaining_in_bucket - 1, queue}},
             remaining_in_window: remaining_in_window - 1
         }}
    end
  end

  # `:next` will run the next `remaining` requests for the given bucket's
  # queue, and stop as soon as no more entries are found, or the user limit has
  # been reached for this window.
  def connected(:internal, {:next, 0, _bucket}, _data) do
    :keep_state_and_data
  end

  def connected(:internal, {:next, _remaining, _bucket}, %{remaining_in_window: 0}) do
    :keep_state_and_data
  end

  # This is the initial request to this bucket, and we want to run the next
  # request. That happens if the request's bucket was not affected by the
  # ratelimits at time of queueing, but the bot was in the user ratelimit
  # state. Treat it as a "we have one request remaining", because that initial
  # request will probe for how many remaining requests we actually have (and
  # the marker value will prevent further requests from executing)
  def connected(:internal, {:next, :initial, bucket}, _data) do
    {:keep_state_and_data, {:next_event, :internal, {:next, 1, bucket}}}
  end

  # Run the next request for the given bucket, with > 0 and non-initial remaining calls.
  def connected(:internal, {:next, remaining, bucket}, %{outstanding: outstanding} = data) do
    # "_remaining" here could be either the `remaining` value from above
    # or `:initial` in the case where we're doing a global-limit requeue.
    {_remaining, queue} = Map.fetch!(outstanding, bucket)

    case :queue.out(queue) do
      {:empty, _queue} ->
        # Nobody wants to run anything on the bucket. We can hop out.
        :keep_state_and_data

      {{:value, {request, from}}, updated_queue} ->
        # We found a request we can queue. Start it, and then try and see if we
        # can queue another one, repeating the cycle until we have either
        # exhausted the queue of waiting requests or the remaining calls on the
        # endpoint.
        accounted_remaining = remaining - 1

        # The `:run` function will take care of updating the remaining calls in
        # the `outstanding` map.
        outstanding_without_this = Map.put(outstanding, bucket, {remaining, updated_queue})

        run_request = {:next_event, :internal, {:run, request, bucket, from}}
        try_starting_next = {:next_event, :internal, {:next, accounted_remaining, bucket}}

        {:keep_state, %{data | outstanding: outstanding_without_this},
         [run_request, try_starting_next]}
    end
  end

  # Requests were recently paused due to global limit. Unpause as many as we
  # can.
  def connected(:internal, {:unpause_requests, [bucket | buckets]}, %{
        remaining_in_window: remaining,
        outstanding: outstanding
      })
      when remaining > 0 do
    {stored_remaining, _queue} = Map.fetch!(outstanding, bucket)
    Logger.warning("Requeueing request to #{inspect(bucket)} after user limit")
    # Due to the way that `:next` is implemented, if we have a large queue on a
    # single bucket we may quickly exhaust the user calls for this second
    # instantly.
    {:keep_state_and_data,
     [
       {:next_event, :internal, {:next, stored_remaining, bucket}},
       {:next_event, :internal, {:unpause_requests, buckets}}
     ]}
  end

  # No more things to unpause.
  def connected(:internal, {:unpause_requests, []}, _data) do
    :keep_state_and_data
  end

  # Hmm, we'll try again later.
  def connected(:internal, {:unpause_requests, buckets}, %{remaining_in_window: 0}) do
    Logger.warning(
      "Unpaused a few requests since the user ratelimit has reset, but #{length(buckets)} bucket(s) are left in the outstanding queue. If the amount of requests stays above the user ratelimit, a backlog may grow"
    )

    :keep_state_and_data
  end

  # Our user timeout has reset - put it back to Discord's documented value.
  # Case 1: We still had some calls remaining, so no request queues paused
  # themselves.
  def connected(
        {:timeout, @bot_calls_timeout_event},
        :expired,
        %{remaining_in_window: remaining} = data
      )
      when remaining > 0 do
    {:keep_state, %{data | remaining_in_window: @bot_calls_per_window}}
  end

  # Case 2: We did not have any calls remaining. In addition to resetting it,
  # we need to kickstart our requests again.
  def connected(
        {:timeout, @bot_calls_timeout_event},
        :expired,
        %{remaining_in_window: 0, outstanding: outstanding} = data
      ) do
    Logger.debug("Received user call window reset with no remaining requests, unpausing")

    {:keep_state, %{data | remaining_in_window: @bot_calls_per_window},
     {:next_event, :internal, {:unpause_requests, Map.keys(outstanding)}}}
  end

  # The bucket's ratelimit window has expired: we may make calls again. Or, to
  # be more specific, we may make a single call to find out how many calls we
  # will have remaining in the next window. If there are waiting entries, we
  # start scheduling, unless we are out of requests for this time window on all
  # bot requests.
  def connected({:timeout, bucket}, :expired, %{remaining_in_window: 0}) do
    Logger.warning(
      "Ratelimits on #{inspect(bucket)} have expired but we may not queue more requests due to the bot user limit."
    )

    :keep_state_and_data
  end

  def connected(
        {:timeout, bucket},
        :expired,
        %{outstanding: outstanding} = data
      )
      when is_map_key(outstanding, bucket) do
    # "remaining" is mostly worthless here, since the bucket's remaining calls
    # have now reset anyways.
    {_remaining, queue} = Map.fetch!(outstanding, bucket)

    case :queue.out(queue) do
      {:empty, _queue} ->
        # Nobody else has anything to queue, so we're good on cleaning up the bucket.
        {:keep_state, %{data | outstanding: Map.delete(outstanding, bucket)}}

      {{:value, {request, from}}, updated_queue} ->
        # There's more where that came from. Update the stored queue and
        # schedule the request to run instantly. Since this is the initial
        # request to get the new ratelimit, we also set the special marker.
        outstanding_with_this = Map.put(outstanding, bucket, {:initial, updated_queue})
        run_request = {:next_event, :internal, {:run, request, bucket, from}}
        {:keep_state, %{data | outstanding: outstanding_with_this}, [run_request]}
    end
  end

  # Beginning of the response. For responses without a body, this is the
  # complete response. For responses with a body, we set up the buffer here. In
  # either case, we parse the retrieved ratelimiting information here.
  def connected(
        :info,
        {:gun_response, _conn, stream, kind, status, headers},
        %{inflight: inflight, running: running} = data
      ) do
    {bucket, request, from} = Map.fetch!(running, stream)
    response = parse_response(status, headers)
    limits = parse_headers(response)
    parse_limits = {:next_event, :internal, {:parse_limits, limits, bucket}}

    cond do
      kind == :fin and status == 429 ->
        # Uh oh. This better be a user or global ratelimit because our
        # ratelimiter is fast, and not because we've actually exhausted a
        # "standard" bucket.
        running_without_this = Map.delete(running, stream)

        Logger.warning(
          "Requeueing request to #{request.method} #{inspect(request.route)} due to 429"
        )

        {:keep_state, %{data | running: running_without_this},
         [
           # parse_limits will transition to the ratelimit state appropriately
           # for us, and event ordering guarantees that this will be the next
           # event that we deal with, regardless of how many clients are
           # sending requests to us in the meantime. Afterwards, the global limit
           # state will need to deal with the requeue request (most likely by
           # postponing it).
           parse_limits,
           {:next_event, :internal, {:requeue, {request, from}, :hit_429}}
         ]}

      kind == :fin ->
        running_without_this = Map.delete(running, stream)

        {:keep_state, %{data | running: running_without_this},
         [
           {:reply, from, format_response(response)},
           parse_limits
         ]}

      kind == :nofin ->
        inflight_with_this = Map.put(inflight, stream, {status, headers, ""})
        {:keep_state, %{data | inflight: inflight_with_this}, parse_limits}
    end
  end

  def connected(:info, {:gun_data, _conn, stream, :nofin, body}, %{inflight: inflight} = data) do
    inflight_with_buffer =
      Map.update!(
        inflight,
        stream,
        fn {status, headers, buffer} ->
          {status, headers, <<buffer::binary, body::binary>>}
        end
      )

    {:keep_state, %{data | inflight: inflight_with_buffer}}
  end

  def connected(
        :info,
        {:gun_data, _conn, stream, :fin, body},
        %{inflight: inflight, running: running} = data
      ) do
    {{_bucket, request, from}, running_without_this} = Map.pop(running, stream)
    {{status, headers, buffer}, inflight_without_this} = Map.pop(inflight, stream)
    full_buffer = <<buffer::binary, body::binary>>
    unparsed = parse_response(status, headers, full_buffer)
    response = format_response(unparsed)
    new_data = %{data | inflight: inflight_without_this, running: running_without_this}

    case status do
      429 ->
        # Not great - this should ideally be a global or user ratelimit, both
        # things which we don't receive any anticipation about from the API.
        # Give the request a second chance. Note that dealing with entering the
        # global or user ratelimit was already performed by `parse_limits` in
        # an earlier step.
        {:keep_state, new_data,
         [
           {:next_event, :internal, {:requeue, {request, from}, :hit_429}}
         ]}

      _ ->
        {:keep_state, new_data, {:reply, from, response}}
    end
  end

  # Parse limits and deal with them accordingly by scheduling the bucket expiry
  # timeout and scheduling the next requests to run as appropriate.
  def connected(
        :internal,
        {:parse_limits, {:bucket_limit, {remaining, reset_after}}, bucket},
        %{outstanding: outstanding} = data
      )
      when remaining >= 0 do
    case Map.fetch(outstanding, bucket) do
      # This is the first response we got for the absolute initial call.
      # Update the remaining value to the reported value.
      {:ok, {:initial, queue}} ->
        updated_outstanding = Map.put(outstanding, bucket, {remaining, queue})

        {:keep_state, %{data | outstanding: updated_outstanding},
         [
           {{:timeout, bucket}, reset_after, :expired},
           {:next_event, :internal, {:next, remaining, bucket}}
         ]}

      # We already have some information about the remaining calls saved. In
      # that case, don't touch it - just try to reschedule and `:next` will do
      # the rest.
      # Why not update the `remaining` value? If we update it to the value
      # reported in the response, it may jump up again, but the remaining
      # value, once set, must strictly monotonically decrease. We count the
      # requests we have running down to zero in the state machine, but when
      # the first response from Discord comes in, it may tell us we have four
      # remaining calls while in reality multiple other requests are already in
      # flight and ready to cause their ratelimiter engineers some headaches.
      # Therefore, we must rely on our own value (and on the API to not decide
      # to change the ratelimit halfway through the bucket lifetime).
      {:ok, {stored_remaining, _queue}} ->
        {:keep_state_and_data,
         [
           {:next_event, :internal, {:next, stored_remaining, bucket}}
         ]}

      # There is no more bucket with outstanding requests anymore.
      # This can happen if the ratelimiter reset - emptying the ratelimiter
      # info - occurs before we receive and parse the response here.
      # In that case, it means that there are also no ratelimits
      # on the route anymore, under the assumption that the ratelimiter's
      # internal remaining -> 0 counting worked properly. In that case,
      # we also don't need to reschedule.
      :error ->
        :keep_state_and_data
    end
  end

  def connected(:internal, {:parse_limits, {:user_limit, retry_after}, _bucket}, data) do
    Logger.warning(
      "Hit user limit, transitioning into global limit state for #{retry_after / 1000} seconds"
    )

    {:next_state, :global_limit, data, [{:state_timeout, retry_after, :connected}]}
  end

  def connected(:internal, {:parse_limits, {:global_limit, retry_after}, _bucket}, data) do
    Logger.warning(
      "Hit global limit, transitioning into global limit state for #{retry_after / 1000} seconds"
    )

    {:next_state, :global_limit, data, [{:state_timeout, retry_after, :connected}]}
  end

  # If we did not get any ratelimit headers let's not send further requests to
  # this endpoint until we get another request to send requests to it. This
  # means that the request queue is effectively paused until another response
  # receives ratelimit headers - so individual requests resulting in 500s
  # (which don't send ratelimit headers) will just stop and not cause a train
  # of hurt. New requests that result in positive feedback will then kick the
  # queue again.
  def connected(:internal, {:parse_limits, :congratulations_you_killed_upstream, bucket}, _data) do
    Logger.warning(
      "No ratelimits received on bucket #{bucket}, likely due to a server error. " <>
        "Holding off request queue pipelining until next client request."
    )

    :keep_state_and_data
  end

  # A running request was killed - suboptimal. Log a warning and try again.
  def connected(:info, {:gun_error, conn, stream, reason}, %{running: running} = data)
      when is_map_key(running, stream) do
    # Ensure that we do not get further garbage for this stream.
    :ok = :gun.cancel(conn, stream)
    :ok = :gun.flush(stream)

    {{_bucket, request, from}, running_without_it} = Map.pop(running, stream)

    Logger.warning(
      "Request to #{inspect(request.route)} queued by #{inspect(from)} was closed abnormally with reason #{reason}, requeueing"
    )

    {:keep_state, %{data | running: running_without_it},
     {:next_event, :internal, {:requeue, {request, from}, :abnormal_close}}}
  end

  def connected(:info, {:gun_down, conn, _, reason, killed_streams}, %{
        running: running,
        wrapped_token: wrapped_token,
        outstanding: outstanding
      }) do
    # Even with `retry: 0`, gun seems to try and reconnect, potentially because
    # of WebSocket. Force the connection to die.
    :ok = :gun.close(conn)
    :ok = :gun.flush(conn)

    # Streams that we previously received `:gun_error` notifications for have
    # been requeued already, and we won't find them in the `running` list.
    # Respond to any client whose request we won't retry.
    # Note that if other code than the `:gun_error` clause for a closed stream
    # removes the request from the `running` map _and does not requeue it on
    # its own terms_, a client may hang indefinitely.
    replies =
      killed_streams
      |> Stream.map(&Map.get(running, &1))
      |> Stream.reject(&(&1 == nil))
      |> Enum.map(fn stream ->
        {_bucket, _request, client} = Map.fetch!(running, stream)
        {:reply, client, {:error, {:connection_died, reason}}}
      end)

    new_data = %{empty_state(wrapped_token.()) | outstanding: outstanding}
    {:next_state, :disconnected, new_data, replies}
  end

  def global_limit(:state_timeout, next, data) do
    {:next_state, next, data}
  end

  # We got a some more data after heading into global limit state. This
  # normally happens when we receive a 429 from a request that is marked
  # `:nofin` (so a body will arrive with it), the internal `parse_limits` event
  # puts us into global limit state, and then the response comes in. Let
  # somebody else deal with it.
  def global_limit(:info, {:gun_response, _conn, _stream, _kind, _status, _headers}, _data) do
    {:keep_state_and_data, :postpone}
  end

  # Same as above.
  def global_limit(:info, {:gun_data, _conn, _stream, _fin_or_nofin, _body}, _data) do
    {:keep_state_and_data, :postpone}
  end

  def global_limit({:call, _from}, {:queue, _request}, _data) do
    {:keep_state_and_data, :postpone}
  end

  # Requeue is sent when a regular request is met with a 429, and this path is
  # always hit at least once after entering the global limit state. Instead of
  # returning an error to the client we postpone it until we can deal with it
  # again.
  def global_limit(:internal, {:requeue, {_request, _from}, _reason}, _data) do
    {:keep_state_and_data, :postpone}
  end

  def global_limit({:timeout, _bucket}, :expired, _data) do
    {:keep_state_and_data, :postpone}
  end

  # End of state functions

  def code_change(_version, state, data, _extra) do
    {:ok, state, data}
  end

  # End of callback functions

  defp get_open_opts do
    default_opts = %{
      connect_timeout: :timer.seconds(5),
      domain_lookup_timeout: :timer.seconds(5),
      # Do not retry here. If we retry, it is possible that after the state
      # machine heads into disconnected state, it receives an unexpected
      # `:gun_up` message. We want the state machine to manage the connection
      # lifecycle fully on its own.
      retry: 0,
      tls_handshake_timeout: :timer.seconds(5),
      tls_opts: Constants.gun_tls_opts()
    }

    if Application.get_env(:nostrum, :force_http1, false) do
      Map.put(default_opts, :protocols, [:http])
    else
      default_opts
    end
  end

  defp parse_response(status, headers), do: {:ok, {status, headers, ""}}

  defp parse_response(status, headers, buffer),
    do: {:ok, {status, headers, buffer}}

  @spec empty_state(String.t()) :: state()
  defp empty_state(token),
    do: %{
      outstanding: %{},
      running: %{},
      inflight: %{},
      conn: nil,
      remaining_in_window: @bot_calls_per_window,
      wrapped_token: fn -> token end
    }

  # Helper functions

  @doc """
  Queue the given request and wait for the response synchronously.

  Ratelimits on the endpoint are handled by the ratelimiter. Global ratelimits
  will cause this to return an error.
  """
  def queue(request) do
    bucket = get_endpoint(request.route, request.method)
    limiter = RatelimiterGroup.limiter_for_bucket(bucket)
    :gen_statem.call(limiter, {:queue, request})
  end

  @spec value_from_rltuple({String.t(), String.t()}) :: String.t() | nil
  defp value_from_rltuple({_k, v}), do: v

  @spec header_value([{String.t(), String.t()}], String.t(), String.t() | nil) :: String.t() | nil
  defp header_value(headers, key, default \\ nil) do
    headers
    |> List.keyfind(key, 0, {key, default})
    |> value_from_rltuple()
  end

  # defp parse_headers({:error, _reason} = result), do: result

  # credo:disable-for-next-line
  defp parse_headers({:ok, {_status, headers, _body}}) do
    limit_scope = header_value(headers, "x-ratelimit-scope")
    remaining = header_value(headers, "x-ratelimit-remaining")
    remaining = unless is_nil(remaining), do: String.to_integer(remaining)

    reset_after = header_value(headers, "x-ratelimit-reset-after")

    reset_after =
      unless is_nil(reset_after), do: :erlang.trunc(String.to_float(reset_after) * 1000)

    cond do
      is_nil(remaining) and is_nil(reset_after) ->
        :congratulations_you_killed_upstream

      limit_scope == "user" and remaining == 0 ->
        # Per bot or user limit.
        {:user_limit, reset_after}

      limit_scope == "global" and remaining == 0 ->
        # Per bot or user global limit.
        {:global_limit, reset_after}

      !is_nil(remaining) and !is_nil(reset_after) ->
        # Normal bucket limit.
        {:bucket_limit, {remaining, reset_after}}
    end
  end

  # Use :timer.time() when / if it is exported
  @spec requeue_after_for_reason(:hit_429 | :abnormal_close) :: pos_integer()
  defp requeue_after_for_reason(:hit_429), do: @retry_429s_after
  defp requeue_after_for_reason(:abnormal_close), do: @retry_abnormal_close_after

  @doc """
  Retrieves a proper ratelimit endpoint from a given route and url.
  """
  @spec get_endpoint(String.t(), atom()) :: String.t()
  def get_endpoint(route, method) do
    endpoint =
      Regex.replace(~r/\/([a-z-]+)\/(?:[0-9]{17,19})/i, route, fn capture, param ->
        case param do
          param when param in @major_parameters ->
            capture

          param ->
            "/#{param}/_id"
        end
      end)
      |> replace_webhook_token()
      |> replace_emojis()

    if String.ends_with?(endpoint, "/messages/_id") and method == :delete do
      "delete:" <> endpoint
    else
      endpoint
    end
  end

  defp format_response(response) do
    case response do
      # {:error, error} ->
      #  {:error, error}

      {:ok, {status, _, body}} when status in [200, 201] ->
        {:ok, body}

      {:ok, {204, _, _}} ->
        {:ok}

      {:ok, {status, _, body}} ->
        response =
          case Jason.decode(body, keys: :atoms) do
            {:ok, parsed} -> parsed
            _error -> body
          end

        {:error, %ApiError{status_code: status, response: response}}
    end
  end

  defp replace_emojis(endpoint) do
    Regex.replace(
      ~r/\/reactions\/[^\/]+\/?(@me|_id)?/i,
      endpoint,
      "/reactions/_emoji/\\g{1}/"
    )
  end

  defp replace_webhook_token(endpoint) do
    Regex.replace(
      ~r/\/webhooks\/([0-9]{17,19})\/[^\/]+\/?/i,
      endpoint,
      "/webhooks/\\g{1}/_token/"
    )
  end
end
