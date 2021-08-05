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
    {:ok, conn_pid} = :gun.open(domain, 443, %{retry: 1_000_000_000})
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

  defp do_request(request, conn) do
    conn
    |> Base.request(request.method, request.route, request.body, request.headers, request.options)
    |> handle_headers(get_endpoint(request.route, request.method))
    |> format_response
  end

  defp handle_headers({:error, reason}, _route), do: {:error, reason}

  defp handle_headers({:ok, {_status, headers, _body}} = response, route) do
    global_limit = headers |> List.keyfind("x-ratelimit-global", 0)
    remaining = headers |> List.keyfind("x-ratelimit-remaining", 0) |> value_from_rltuple
    reset = headers |> List.keyfind("x-ratelimit-reset", 0) |> value_from_rltuple
    retry_after = headers |> List.keyfind("retry-after", 0) |> value_from_rltuple

    origin_timestamp =
      headers
      |> List.keyfind("date", 0)
      |> value_from_rltuple
      |> date_string_to_unix

    latency = abs(origin_timestamp - Util.now())

    if global_limit, do: update_global_bucket(route, 0, retry_after, latency)
    if reset, do: update_bucket(route, remaining, reset, latency)

    response
  end

  defp update_bucket(route, remaining, reset_time, latency) do
    Bucket.update_bucket(route, remaining, reset_time * 1000, latency)
  end

  defp update_global_bucket(_route, _remaining, retry_after, latency) do
    Bucket.update_bucket("GLOBAL", 0, retry_after + Util.now(), latency)
  end

  defp wait_for_timeout(request, timeout, from) do
    Logger.info(
      "RATELIMITER: Waiting #{timeout}ms to process request with route #{request.route}"
    )

    Process.sleep(timeout)
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

  defp value_from_rltuple(tuple) when is_nil(tuple), do: nil
  defp value_from_rltuple({"date", v}), do: v
  defp value_from_rltuple({_k, v}), do: String.to_integer(v)

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

    if String.ends_with?(endpoint, "/messages/_id") and method == "DELETE" do
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
        {:error, %ApiError{status_code: status, response: Poison.decode!(body, keys: :atoms)}}
    end
  end
end
