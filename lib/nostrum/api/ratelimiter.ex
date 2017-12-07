defmodule Nostrum.Api.Ratelimiter do
  @moduledoc """
  Ratelimit implimentation specific to Discord's API.
  Only to be used when starting in a rest-only manner.
  """

  use GenServer

  alias Nostrum.Api.{Base, Bucket}
  alias Nostrum.Util

  require Logger

  @typedoc """
  Return values of start functions.
  """
  @type on_start :: {:ok, pid} |
                    :ignore |
                    {:error, {:already_started, pid} | term}

  @gregorian_epoch 62_167_219_200
  @sanity_wait 500

  @doc """
  Starts the ratelimiter.
  """
  @spec start_link([]) :: on_start
  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: Ratelimiter)
  end

  def init([]) do
    :ets.new(:ratelimit_buckets, [:set, :public, :named_table])
    {:ok, []}
  end

  @doc """
  Empties all buckets, voiding any saved ratelimit values.
  """
  @spec empty_buckets() :: :ok
  def empty_buckets do
    :ets.delete_all_objects(:ratelimit_buckets)
  end

  def handle_call({:queue, request, original_from}, from, state) do
    retry_time = 
      request.route
      |> major_parameter
      |> Bucket.get_ratelimit_timeout

    case retry_time do
      # No wait time, delegate to task
      nil ->
        Task.start(fn ->
          GenServer.reply(original_from || from, do_request(request))
        end)
      # No rl data, block requests until rl data obtained
      :none ->
        GenServer.reply(original_from || from, do_request(request))
      # Wait time found, delegate to task to retry
      time ->
        Task.start(fn ->
          wait_for_timeout(request, time, original_from || from)
        end)
    end

    {:noreply, state}
  end

  defp do_request(request) do
    request.method
    |> Base.request(request.route, request.body, request.headers, request.options)
    |> handle_headers(major_parameter(request.route))
    |> format_response
  end

  defp handle_headers({:error, reason}, _route), do: {:error, reason}
  defp handle_headers({:ok, %HTTPoison.Response{headers: headers}} = response, route) do
    global_limit = headers |> List.keyfind("X-RateLimit-Global", 0)
    remaining = headers |> List.keyfind("X-RateLimit-Remaining", 0) |> value_from_rltuple
    reset = headers |> List.keyfind("X-RateLimit-Reset", 0) |> value_from_rltuple
    retry_after = headers |> List.keyfind("Retry-After", 0) |> value_from_rltuple

    origin_timestamp = 
      headers
      |> List.keyfind("Date", 0)
      |> value_from_rltuple
      |> date_string_to_unix

    latency = abs(origin_timestamp - Util.now)

    if global_limit do
      update_global_bucket(route, 0, retry_after, latency)
    else
      update_bucket(route, remaining, reset, latency)
    end

    response
  end

  defp update_bucket(_route, remaining, _time, _latency) when is_nil(remaining), do: nil
  defp update_bucket(route, remaining, reset_time, latency) do
    Bucket.create_bucket(route, remaining, reset_time * 1000, latency)
  end

  defp update_global_bucket(_route, _remaining, retry_after, latency) do
    Bucket.create_bucket("GLOBAL", 0, retry_after + Util.now, latency)
  end

  defp wait_for_timeout(request, timeout, from) do
    Logger.info "RATELIMITER: Waiting #{timeout}ms to process request with route #{request.route}"
    Process.sleep(timeout + @sanity_wait) # Small wait for sanity sake
    GenServer.call(Ratelimiter, {:queue, request, from}, :infinity)
  end

  defp date_string_to_unix(header) do
    header
    |> String.to_charlist
    |> :httpd_util.convert_request_date
    |> erl_datetime_to_timestamp
  end

  defp erl_datetime_to_timestamp(datetime) do
    (:calendar.datetime_to_gregorian_seconds(datetime) - @gregorian_epoch) * 1000
  end

  defp value_from_rltuple(tuple) when is_nil(tuple), do: nil
  defp value_from_rltuple({"Date", v}), do: v
  defp value_from_rltuple({_k, v}), do: String.to_integer v

  defp major_parameter(route) do
    case Regex.run(~r/\/(channels|guilds)\/([0-9]{15,})+/i, route) do
      [match, _route, _major_param] -> match
      nil -> route
    end
  end

  defp format_response(response) do
    case response do
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, %{status_code: nil, message: reason}}
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: 204}} ->
        {:ok}
      {:ok, %HTTPoison.Response{status_code: status_code, body: body}} ->
        {:error, %{status_code: status_code, message: Poison.decode!(body)}}
    end
  end

end
