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
    remaining = headers |> List.keyfind("X-RateLimit-Remaining", 0) |> value_from_rltuple
    reset = headers |> List.keyfind("X-RateLimit-Reset", 0) |> value_from_rltuple
    retry_after = headers |> List.keyfind("Retry-After", 0) |> value_from_rltuple
    origin_timestamp = headers
      |> List.keyfind("Date", 0)
      |> value_from_rltuple
      |> date_string_to_unix
    IO.inspect(remaining)
    update_buckets(route, remaining, reset, retry_after, origin_timestamp)
    response
  end

  def update_buckets(route, remaining, reset, retry_after, origin_timestamp) when is_nil(retry_after) and not is_nil(remaining) do
    IO.inspect "HERE"
    Bucket.create_bucket(route, remaining, reset - origin_timestamp)
  end

  def update_buckets(route, remaining, reset, retry_after, origin_timestamp) when not is_nil(retry_after) do
    IO.inspect "THERE"
    Bucket.create_bucket("GLOBAL", 0, Float.ceil(retry_after / 1000) |> Kernel.round)
  end

  def update_buckets(_route, _remaining, _reset, _retry_after, _origin_timestamp), do: nil

  def wait_for_timeout(request, timeout, from) do
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
    :calendar.datetime_to_gregorian_seconds(datetime) - 62167219200
  end

  defp value_from_rltuple(tuple) when is_nil(tuple), do: nil
  defp value_from_rltuple({"Date", v}), do: v
  defp value_from_rltuple({k, v}), do: String.to_integer v

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