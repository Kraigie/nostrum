defmodule Nostrum.Api.Ratelimiter do
  @moduledoc """
  Handles REST calls to the Discord API while respecting ratelimits.

  ## Purpose

  Discord's API returns information about ratelimits that we must respect. This
  module performs serialization of these requests through a single process,
  thus preventing concurrency issues from arising if two processes make a
  remote API call at the same time.

  ## Inner workings

  When a client process wants to perform some request on the Discord API, it
  sends a request to the `GenServer` behind this module to ask it to `:queue`
  the incoming request.

  The server looks up the ratelimit buckets for the given endpoint using the
  configured `Nostrum.Store.RatelimitBucket`. If no bucket is available, a
  request will be sent out directly, and the server will wait for the response.

  After receiving a response, the ratelimiter updates the matching ratelimit
  bucket and return the response to the client.

  If the client disconnects from the ratelimiter, or the request is dropped by
  the ratelimiter for another reason - usually a timeout - while the request is
  still existing on Discord's end, the Ratelimiter will log the response later
  when it receives it.

  ### Serialization and buckets

  We serialize all REST requests in nostrum through this process to prevent
  concurrency issues arising from multiple clients exhausting the bucket (e.g.
  going to a `t:Nostrum.Store.RatelimitBucket.remaining/0` value below `0`).
  This critical spot only needs to happen shortly before running the request,
  but **only if we already have a bucket for the coming request**. If we do not
  have a bucket already, we must serialize it and not make further requests for
  the same bucket until we have received information from Discord on the
  ratelimits on the given endpoint. Otherwise, we may end up running multiple
  requests to the same endpoint because no bucket was stored to tell us that we
  shouldn't. A more efficient alternative may be only blocking requests to the
  specific bucket we have sent a request to by keeping track of "unbucketed
  running requests" and removing elements as we retrieve bucket information.
  """

  use GenServer

  alias Nostrum.Api.Base
  alias Nostrum.Constants
  alias Nostrum.Error.ApiError
  alias Nostrum.Store.RatelimitBucket
  alias Nostrum.Util

  require Logger

  # Ratelimits are waited out on client-side. This constant determines the
  # attempts to requeue an individual request if the ratelimiter told us that
  # we should wait. Once this many attempts have been made at queueing a
  # request, further attempts are aborted.
  @default_attempts_to_requeue 50

  # Total attempts to try to reconnect to the API in case it is not reachable.
  # The current amount of reconnect attempts is intentionally geared to
  # basically reconnect forever, because the ratelimiter process currently
  # always assumes a working connection. The `handle_info` callbacks for the
  # `:gun_up` and `:gun_down` events may be of interest.
  @reconnect_attempts 1_000_000_000

  # How often to prune stale buckets in the ratelimiter bucket storage.
  @bucket_cleanup_interval :timer.hours(1)

  # How far back to prune when running a stale bucket cleanup. Any ratelimiter
  # buckets older than the interval given here will be removed every
  # `@bucket_cleanup_interval` milliseconds.
  @bucket_cleanup_window @bucket_cleanup_interval

  @typedoc """
  Return values of start functions.
  """
  @type on_start ::
          {:ok, pid}
          | :ignore
          | {:error, {:already_started, pid} | term}

  @major_parameters ["channels", "guilds", "webhooks"]
  @gregorian_epoch 62_167_219_200
  @registered_name __MODULE__

  @doc """
  Starts the ratelimiter.
  """
  @spec start_link([]) :: on_start
  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: @registered_name)
  end

  def init([]) do
    domain = to_charlist(Constants.domain())

    open_opts = %{retry: @reconnect_attempts, tls_opts: Constants.gun_tls_opts()}
    {:ok, conn_pid} = :gun.open(domain, 443, open_opts)

    {:ok, :http2} = :gun.await_up(conn_pid)

    # Start the old route cleanup loop
    Process.send_after(self(), :remove_old_buckets, @bucket_cleanup_interval)

    {:ok, conn_pid}
  end

  defp get_retry_time(route, method) do
    route
    |> get_endpoint(method)
    |> RatelimitBucket.timeout_for()
  end

  @doc """
  Queue the given request.

  If the ratelimiter tells us to sit it out and we have more than `0` attempts
  remaining, we sleep out the given retry time and ask it to queue again
  afterwards.
  """
  def queue(request, attempts_remaining \\ @default_attempts_to_requeue) do
    case GenServer.call(@registered_name, {:queue, request}) do
      {:error, {:retry_after, time}} when attempts_remaining > 0 ->
        truncated = :erlang.ceil(time)

        attempt = @default_attempts_to_requeue - attempts_remaining + 1

        Logger.info(
          "RATELIMITER: Waiting #{truncated}ms to process request with route #{request.route} (try #{attempt} / #{@default_attempts_to_requeue})"
        )

        Process.sleep(truncated)
        queue(request, attempts_remaining - 1)

      # We've had enough. Bail.
      {:error, {:retry_after, _time}} when attempts_remaining == 0 ->
        {:error, :max_retry_attempts_exceeded}

      result ->
        result
    end
  end

  def handle_call({:queue, request}, _from, conn) do
    # Do the final serialized double-take of ratelimits to prevent races.
    # The client already did it, but, you know.
    case get_retry_time(request.route, request.method) do
      :now ->
        {:reply, do_request(request, conn), conn}

      time ->
        {:reply, {:error, {:retry_after, time}}, conn}
    end
  end

  # The gun connection went down. Any requests in `_killed_streams` are definitely gone.
  # Other streams may also be gone. Gun will reconnect automatically for us.
  def handle_info({:gun_down, _conn, _proto, _reason, _killed_streams}, state) do
    {:noreply, state}
  end

  # Gun automatically reconnected after the connection went down previously.
  def handle_info({:gun_up, _conn, _proto}, state) do
    {:noreply, state}
  end

  def handle_info({:gun_response, _conn, _ref, :nofin, status, _headers}, state) do
    Logger.debug(
      "Got unexpected (probably late) HTTP response with status #{status}, discarding it"
    )

    {:noreply, state}
  end

  def handle_info({:gun_data, _conn, _ref, _conn_state, _data}, state) do
    Logger.debug("Got unexpected (probably late) data from a HTTP response, discarding it")
    {:noreply, state}
  end

  def handle_info(:remove_old_buckets, state) do
    RatelimitBucket.cleanup(@bucket_cleanup_window)
    Process.send_after(self(), :remove_old_buckets, @bucket_cleanup_interval)
    {:noreply, state}
  end

  defp do_request(request, conn) do
    conn
    |> Base.request(request.method, request.route, request.body, request.headers, request.params)
    |> handle_headers(get_endpoint(request.route, request.method))
    |> format_response
  end

  @spec value_from_rltuple({String.t(), String.t()}) :: String.t() | nil
  defp value_from_rltuple({_k, v}), do: v

  @spec header_value([{String.t(), String.t()}], String.t(), String.t() | nil) :: String.t() | nil
  defp header_value(headers, key, default \\ nil) do
    headers
    |> List.keyfind(key, 0, {key, default})
    |> value_from_rltuple()
  end

  defp handle_headers({:error, reason}, _route), do: {:error, reason}

  defp handle_headers({:ok, {_status, headers, _body}} = response, route) do
    # Per https://discord.com/developers/docs/topics/rate-limits, all of these
    # headers are optional, which is why we supply a default of 0.

    global_limit = header_value(headers, "x-ratelimit-global")

    remaining = header_value(headers, "x-ratelimit-remaining")
    remaining = unless is_nil(remaining), do: String.to_integer(remaining)

    reset = header_value(headers, "x-ratelimit-reset")
    reset = unless is_nil(reset), do: :erlang.trunc(:math.ceil(String.to_float(reset)))
    retry_after = header_value(headers, "retry-after")

    retry_after =
      unless is_nil(retry_after) do
        # Since for some reason this might not contain a "."
        # and String.to_float raises if it doesn't
        {retry_after, ""} = Float.parse(retry_after)
        :erlang.trunc(:math.ceil(retry_after))
      end

    origin_timestamp =
      headers
      |> header_value("date", "0")
      |> date_string_to_unix

    latency = abs(origin_timestamp - Util.now())

    # If we have hit a global limit, Discord responds with a 429 and informs
    # us when we can retry. Our global bucket keeps track of this ratelimit.
    unless is_nil(global_limit), do: update_global_bucket(route, 0, retry_after, latency)

    # If Discord did send us other ratelimit information, we can also update
    # the ratelimiter bucket for this route. For some endpoints, such as
    # when creating a DM with a user, we may not retrieve ratelimit headers.
    unless is_nil(reset) or is_nil(remaining), do: update_bucket(route, remaining, reset, latency)

    response
  end

  defp update_bucket(route, remaining, reset_time, latency) do
    RatelimitBucket.update(route, remaining, reset_time * 1000, latency)
  end

  defp update_global_bucket(_route, _remaining, retry_after, latency) do
    RatelimitBucket.update("GLOBAL", 0, retry_after + Util.now(), latency)
  end

  defp date_string_to_unix(header) do
    header
    |> String.to_charlist()
    |> :httpd_util.convert_request_date()
    |> erl_datetime_to_timestamp
  end

  defp erl_datetime_to_timestamp(datetime) do
    (:calendar.datetime_to_gregorian_seconds(datetime) - @gregorian_epoch) * 1000
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
      {:error, error} ->
        {:error, error}

      {:ok, {status, _, body}} when status in [200, 201] ->
        {:ok, body}

      {:ok, {204, _, _}} ->
        {:ok}

      {:ok, {status, _, body}} ->
        {:error, %ApiError{status_code: status, response: Jason.decode!(body, keys: :atoms)}}
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
