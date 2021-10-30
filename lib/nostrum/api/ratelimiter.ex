defmodule Nostrum.Api.Ratelimiter do
  @moduledoc """
  Ratelimit implimentation specific to Discord's API.
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

  @doc """
  Starts the ratelimiter.
  """
  @spec start_link([]) :: on_start
  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: Ratelimiter)
  end

  def init([]) do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    domain = to_charlist(Constants.domain())

    open_opts = %{retry: 1_000_000_000, tls_opts: Constants.gun_tls_opts()}
    {:ok, conn_pid} = :gun.open(domain, 443, open_opts)

    {:ok, :http2} = :gun.await_up(conn_pid)
    {:ok, conn_pid}
  end

  @doc """
  Empties all buckets, voiding any saved ratelimit values.
  """
  @spec empty_buckets() :: true
  def empty_buckets do
    :ets.delete_all_objects(:ratelimit_buckets)
  end

  def handle_call({:queue, request, original_from}, from, conn) do
    retry_time =
      request.route
      |> get_endpoint(request.method)
      |> Bucket.get_ratelimit_timeout()

    case retry_time do
      :now ->
        GenServer.reply(original_from || from, do_request(request, conn))

      time when time < 0 ->
        GenServer.reply(original_from || from, do_request(request, conn))

      time ->
        Task.start(fn ->
          wait_for_timeout(request, time, original_from || from)
        end)
    end

    {:noreply, conn}
  end

  def handle_info({:gun_down, _conn, _proto, _reason, _killed_streams}, state) do
    {:noreply, state}
  end

  def handle_info({:gun_up, _conn, _proto}, state) do
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
    remaining = header_value(headers, "x-ratelimit-remaining", "0") |> String.to_integer()
    reset = header_value(headers, "x-ratelimit-reset", "0.0") |> String.to_float()
    retry_after = header_value(headers, "retry-after", "0") |> String.to_integer()

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
    if reset != 0 and remaining != 0, do: update_bucket(route, remaining, reset, latency)

    response
  end

  defp update_bucket(route, remaining, reset_time, latency) do
    Bucket.update_bucket(route, remaining, reset_time * 1000, latency)
  end

  defp update_global_bucket(_route, _remaining, retry_after, latency) do
    Bucket.update_bucket("GLOBAL", 0, retry_after + Util.now(), latency)
  end

  defp wait_for_timeout(request, timeout, from) do
    truncated = :erlang.ceil(timeout)

    Logger.info(
      "RATELIMITER: Waiting #{truncated}ms to process request with route #{request.route}"
    )

    Process.sleep(truncated)
    GenServer.call(Ratelimiter, {:queue, request, from}, :infinity)
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
end
