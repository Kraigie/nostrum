defmodule Mixcord.Api.Ratelimiter do
  @moduledoc false

  use GenServer
  alias Mixcord.Api.{Base, Bucket}
  alias Mixcord.Util

  def start_link do
    GenServer.start_link(__MODULE__, [], name: Ratelimiter)
  end

  def handle_call({:queue, request, original_from}, from, state) do
    retry_time = request.route
      |> Bucket.get_ratelimit_timeout
    if retry_time do
      Task.start(fn -> wait_for_timeout(request, retry_time, original_from || from) end)
    else
      response = request.method
        |> Base.request(request.route, request.body, [], request.options)
        |> handle_headers(request.route)
        |> format_response
      GenServer.reply(original_from || from, response)
    end

    {:noreply, state}
  end

  def handle_headers({:ok, %HTTPoison.Response{headers: headers}} = response, route) do
    global_limit = headers |> List.keyfind("X-RateLimit-Global", 0)
    remaining = headers |> List.keyfind("X-RateLimit-Remaining", 0) |> value_from_rltuple
    reset = headers |> List.keyfind("X-RateLimit-Reset", 0) |> value_from_rltuple
    retry_after = headers |> List.keyfind("Retry-After", 0) |> value_from_rltuple
    origin_timestamp = headers
      |> List.keyfind("Date", 0)
      |> value_from_rltuple
      |> date_string_to_unix
    latency = abs(origin_timestamp - Util.now)

    if global_limit do
      update_global_bucket(route, remaining, retry_after, latency)
    end

    update_bucket(route, remaining, reset, latency)
    response
  end

  def update_bucket(_route, remaining, _time, _latency) when is_nil(remaining), do: nil
  def update_bucket(route, remaining, reset_time, latency) do
    Bucket.create_bucket(route, remaining, reset_time * 1000, latency)
  end

  def update_global_bucket(_route, _remaining, retry_after, latency) do
    Bucket.create_bucket("GLOBAL", 0, retry_after + Util.now, latency)
  end

  def wait_for_timeout(request, timeout, from) do
    IO.inspect("ABOUT TO WAIT #{timeout}")
    Process.sleep(timeout + 500) # Small wait for sanity sake
    GenServer.call(Ratelimiter, {:queue, request, from}, :infinity)
  end

  def date_string_to_unix(header) do
    header
      |> String.to_charlist
      |> :httpd_util.convert_request_date
      |> erl_datetime_to_timestamp
  end

  defp erl_datetime_to_timestamp(datetime) do
    (:calendar.datetime_to_gregorian_seconds(datetime) - 62167219200) * 1000
  end

  defp value_from_rltuple(tuple) when is_nil(tuple), do: nil
  defp value_from_rltuple({"Date", v}), do: v
  defp value_from_rltuple({_k, v}), do: String.to_integer v

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