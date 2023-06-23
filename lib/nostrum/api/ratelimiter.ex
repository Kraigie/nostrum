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

  If a single global process is desired to handle all ratelimiting, the
  ratelimiter can theoretically be adjusted to start registered via `:global`.
  In practice, it may be more beneficial to have a local ratelimiter process on
  each node and either using the local one for any API calls, or using a
  consistent hash mechanism to distribute API requests around the cluster as
  needed.


  ## Inner workings

  When a client process wants to perform some request on the Discord API, it
  sends a request to the `:gen_statem` behind this module to ask it to `:queue`
  the incoming request.


  ### Connection setup

  If the state machine is not connected to the HTTP endpoint, it will
  transition to the `:connecting` state and try to open the connection. If this
  succeeds, it transitions to the `:connected` state.

  The state machine associates a `t::queue.queue/1` of `t:queued_request/0` to
  each individual bucket, together with an internal count of remaining calls.
  If an entry is found with remaining calls above 0, the request is scheduled
  for immediate execution. If an entry is found with remaining calls of 0, or
  with the special `:initial` value (indicating that the initial request to
  find the ratelimit just headed out), it is queued. Otherwise, if no entry is
  found, a new queue is created with an `:initial` remaining call count, and
  the request scheduled for immediate execution.

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

  - The queue is empty: Delete the queue and remaining calls from the outstanding buckets.

  In practice, this means that we never store more information than we need,
  and removes the previous regular bucket sweeping functionality that the
  ratelimit buckets required.

  **Global ratelimits** are handled with the special `global_limit` state. This
  state is only entered with a state timeout, the state timeout being the
  `X-Ratelimit-Reset-After` value provided in the global ratelimit response.
  This state does nothing apart from postponing any events it receives and
  returning to the previous state (`:connected`) once the global timeout is
  gone.


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

  alias Nostrum.Api.Base
  alias Nostrum.Constants
  alias Nostrum.Error.ApiError

  require Logger

  @major_parameters ["channels", "guilds", "webhooks"]
  @registered_name __MODULE__

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
          conn: pid() | nil
        }

  @doc """
  Starts the ratelimiter.
  """
  @spec start_link([:gen_statem.start_opt()]) :: :gen_statem.start_ret()
  def start_link(opts) do
    :gen_statem.start_link({:local, @registered_name}, __MODULE__, [], opts)
  end

  def init([]) do
    # Uncomment the following to trace everything the ratelimiter is doing:
    #   me = self()
    #   spawn(fn -> :sys.trace(me, true) end)
    # See more examples in the `sys` docs.
    {:ok, :disconnected, empty_state()}
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

    open_opts = %{
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

    {:ok, conn_pid} = :gun.open(domain, 443, open_opts)
    {:keep_state, %{data | conn: conn_pid}}
  end

  def connecting(:info, {:gun_up, conn_pid, :http2}, %{conn: conn_pid} = data) do
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

  def connected({:call, from}, {:queue, request}, %{outstanding: outstanding} = data) do
    bucket = get_endpoint(request.route, request.method)

    # The outstanding maps contains pairs in the form `{remaining, queue}`,
    # where `remaining` is the amount of remaining calls we may make, and
    # `queue` is the waiting line of requests. If the ratelimit on the bucket
    # expires, the internal timeout event will automatically reschedule queued
    # requests (starting with a single one to get the calls we may make).
    case Map.get(outstanding, bucket) do
      # We have no remaining calls, or the initial call to get rate limiting
      # information is in flight. Let's join the waiting line.
      {remaining, queue} when remaining in [0, :initial] ->
        entry = {request, from}

        data_with_this_queued =
          put_in(data, [:outstanding, bucket], {remaining, :queue.in(entry, queue)})

        {:keep_state, data_with_this_queued}

      # There is an entry - so somebody did find some ratelimiting information
      # here recently - but that entry tells us we may make a call right away.
      {remaining, queue} when remaining > 0 ->
        # Sanity check. This can be removed after release is considered stable.
        # Why should this be empty?
        # Because when we receive ratelimit information and see that there are
        # still items in the queue, we should internally schedule them right away.
        # Otherwise, we are mixing up the order.
        true = :queue.is_empty(queue)
        {:keep_state_and_data, [{:next_event, :internal, {:run, request, bucket, from}}]}

      # There is no entry. We are the pioneer for this bucket.
      nil ->
        # Since we don't have any explicit ratelimiting information for this
        # bucket yet, we set the remaining calls to the special `:initial`
        # value. The ratelimit response header parser uses this value to know
        # when it should update ratelimit information from upstream, and new
        # incoming requests will be held off appropriately.
        run_request = {:next_event, :internal, {:run, request, bucket, from}}
        data_with_new_queue = put_in(data, [:outstanding, bucket], {:initial, :queue.new()})
        {:keep_state, data_with_new_queue, [run_request]}
    end
  end

  def connected(:internal, {:requeue, {request, from}}, data) do
    Logger.warning("Requeueing request to #{request.method} #{inspect(request.route)} due to 429")
    connected({:call, from}, {:queue, request}, data)
  end

  # Run the given request right now, and do any bookkeeping.
  def connected(:internal, {:run, request, bucket, from}, %{conn: conn} = data) do
    stream =
      Base.request(
        conn,
        request.method,
        request.route,
        request.body,
        request.headers,
        request.params
      )

    data_with_this_running = put_in(data, [:running, stream], {bucket, request, from})
    {:keep_state, data_with_this_running}
  end

  # `:next` will run the next `remaining` requests for the given bucket's
  # queue, and stop as soon as no more entries are found.
  def connected(:internal, {:next, 0, _bucket}, _data) do
    :keep_state_and_data
  end

  def connected(:internal, {:next, remaining, bucket}, %{outstanding: outstanding} = data) do
    {^remaining, queue} = Map.fetch!(outstanding, bucket)

    case :queue.out(queue) do
      {:empty, _queue} ->
        # Nobody wants to run anything on the bucket. We can hop out.
        :keep_state_and_data

      {{:value, {request, from}}, updated_queue} ->
        # We found a request we can queue. Account for the request, start it,
        # and then try and see if we can queue another one, repeating the cycle
        # until we have either exhausted the queue of waiting requests or the
        # remaining calls on the endpoint.
        accounted_remaining = remaining - 1

        outstanding_without_this =
          Map.put(outstanding, bucket, {accounted_remaining, updated_queue})

        run_request = {:next_event, :internal, {:run, request, bucket, from}}
        try_starting_next = {:next_event, :internal, {:next, accounted_remaining, bucket}}

        {:keep_state, %{data | outstanding: outstanding_without_this},
         [run_request, try_starting_next]}
    end
  end

  # The bucket's ratelimit window has expired: we may make calls again. Or, to
  # be more specific, we may make a single call to find out how many calls we
  # will have remaining in the next window. If there are waiting entries, we
  # start scheduling.
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

        {:keep_state, %{data | running: running_without_this},
         [
           # parse_limits will transition to the ratelimit state appropriately
           # for us, and event ordering guarantees that this will be the next
           # event that we deal with, regardless of how many clients are
           # sending requests to us in the meantime. Afterwards, the global limit
           # state will need to deal with the requeue request (most likely by
           # postponing it).
           parse_limits,
           {:next_event, :internal, {:requeue, {request, from}}}
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
        # Give the request a second chance.
        {:keep_state, new_data,
         [
           {:next_event, :internal, {:requeue, {request, from}}}
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
    expire_bucket = {{:timeout, bucket}, reset_after, :expired}

    case Map.fetch(outstanding, bucket) do
      # This is the first response we got for the absolute initial call.
      # Update the remaining value to the reported value.
      {:ok, {:initial, queue}} ->
        updated_outstanding = Map.put(outstanding, bucket, {remaining, queue})

        {:keep_state, %{data | outstanding: updated_outstanding},
         [
           expire_bucket,
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
           expire_bucket,
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

  def connected(:info, {:gun_down, conn, :http2, reason, killed_streams}, %{running: running}) do
    # Even with `retry: 0`, gun seems to try and reconnect, potentially because
    # of WebSocket. Force the connection to die.
    :ok = :gun.close(conn)
    :ok = :gun.flush(conn)

    replies =
      Enum.map(
        killed_streams,
        fn stream ->
          {_bucket, _request, client} = Map.fetch!(running, stream)
          {:reply, client, {:error, {:connection_died, reason}}}
        end
      )

    {:next_state, :disconnected, empty_state(), replies}
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

  # Requeue is sent when a regular request is met with a 429. Instead of
  # returning an error to the client we put it back into the queue and retry
  # later.
  def global_limit(:internal, {:requeue, {request, from}}, data) do
    global_limit({:call, from}, {:queue, request}, data)
  end

  def global_limit({:timeout, _bucket}, :expired, _data) do
    {:keep_state_and_data, :postpone}
  end

  # End of state functions

  def code_change(_version, state, data, _extra) do
    {:ok, state, data}
  end

  # End of callback functions

  defp parse_response(status, headers), do: {:ok, {status, headers, ""}}

  defp parse_response(status, headers, buffer),
    do: {:ok, {status, headers, buffer}}

  @spec empty_state :: state()
  defp empty_state,
    do: %{
      outstanding: %{},
      running: %{},
      inflight: %{},
      conn: nil
    }

  # Helper functions

  @doc """
  Queue the given request and wait for the response synchronously.

  Ratelimits on the endpoint are handled by the ratelimiter. Global ratelimits
  will cause this to return an error.
  """
  def queue(request) do
    :gen_statem.call(@registered_name, {:queue, request})
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

  defp parse_headers({:ok, {_status, headers, _body}}) do
    limit_scope = header_value(headers, "x-ratelimit-scope")
    remaining = header_value(headers, "x-ratelimit-remaining")
    remaining = unless is_nil(remaining), do: String.to_integer(remaining)

    reset_after = header_value(headers, "x-ratelimit-reset-after")

    cond do
      is_nil(remaining) and is_nil(reset_after) ->
        :congratulations_you_killed_upstream

      # We should add an internal timer for the following to properly track:
      # https://discord.com/developers/docs/topics/rate-limits
      # "All bots can make up to 50 requests per second to our API."
      limit_scope == "user" and remaining == 0 ->
        # Per bot or user limit.
        {:user_limit, reset_after}

      limit_scope == "global" and remaining == 0 ->
        # Per bot or user global limit.
        {:global_limit, reset_after}

      !is_nil(remaining) and !is_nil(reset_after) ->
        parsed_reset = :erlang.trunc(String.to_float(reset_after) * 1000)
        {:bucket_limit, {remaining, parsed_reset}}
    end
  end

  @doc """
  Retrieves a proper ratelimit endpoint from a given route and url.
  """
  @spec get_endpoint(String.t(), String.t()) :: String.t()
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
