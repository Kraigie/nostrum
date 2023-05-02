defmodule Nostrum.Api.Ratelimiter do
  @moduledoc """
  Ratelimit implementation specific to Discord's API.
  Only to be used when starting in a rest-only manner.
  """

  use GenServer

  alias Nostrum.Api.{Base, Bucket}
  alias Nostrum.Constants
  alias Nostrum.Error.ApiError
  alias Nostrum.Util

  require Logger

  @typedoc """
  Return values of start functions.
  """
  @type on_start ::
          {:ok, pid}
          | :ignore
          | {:error, {:already_started, pid} | term}

  @major_parameters ["channels", "guilds", "webhooks"]
  @gregorian_epoch 62_167_219_200
  @registered_name Ratelimiter

  @doc """
  Starts the ratelimiter.
  """
  @spec start_link([]) :: on_start
  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: @registered_name)
  end

  def init([]) do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    domain = to_charlist(Constants.domain())

    open_opts = %{retry: 1_000_000_000, tls_opts: Constants.gun_tls_opts()}
    {:ok, conn_pid} = :gun.open(domain, 443, open_opts)

    {:ok, :http2} = :gun.await_up(conn_pid)

    # Start the old route cleanup loop
    Process.send_after(self(), :remove_old_buckets, :timer.hours(1))

    {:ok, conn_pid}
  end

  @doc """
  Empties all buckets, voiding any saved ratelimit values.
  """
  @spec empty_buckets() :: true
  def empty_buckets do
    :ets.delete_all_objects(:ratelimit_buckets)
  end

  defp get_retry_time(route, method) do
    route
    |> get_endpoint(method)
    |> Bucket.get_ratelimit_timeout()
  end

  def queue(request) do
    case GenServer.call(@registered_name, {:queue, request}) do
      {:error, {:retry_after, time}} ->
        truncated = :erlang.ceil(time)

        Logger.info(
          "RATELIMITER: Waiting #{truncated}ms to process request with route #{request.route}"
        )

        Process.sleep(truncated)
        GenServer.call(@registered_name, {:queue, request})

      result ->
        result
    end
  end

  def handle_call({:queue, request}, _from, conn) do
    case get_retry_time(request.route, request.method) do
      :now ->
        {:reply, do_request(request, conn), conn}

      time when time < 0 ->
        {:reply, do_request(request, conn), conn}

      time ->
        {:reply, {:error, {:retry_after, time}}, conn}
    end
  end

  def handle_info({:gun_down, _conn, _proto, _reason, _killed_streams}, state) do
    {:noreply, state}
  end

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
    Bucket.remove_old_buckets()
    Process.send_after(self(), :remove_old_buckets, :timer.hours(1))
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
    reset = unless is_nil(reset), do: String.to_float(reset)
    retry_after = header_value(headers, "retry-after")

    retry_after =
      unless is_nil(retry_after) do
        # Since for some reason this might not contain a "."
        # and String.to_float raises if it doesn't
        {retry_after, ""} = Float.parse(retry_after)
        retry_after
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
    Bucket.update_bucket(route, remaining, reset_time * 1000, latency)
  end

  defp update_global_bucket(_route, _remaining, retry_after, latency) do
    Bucket.update_bucket("GLOBAL", 0, retry_after + Util.now(), latency)
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
