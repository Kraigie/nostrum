defmodule Mixcord.Api.Ratelimiter do
  @moduledoc false

  use GenServer

  alias Mixcord.Api.{Base, Bucket}
  alias Mixcord.Util

  def start_link do
    GenServer.start_link(__MODULE__, [], name: Ratelimiter)
  end

  def request(method, route, body \\ "", options \\ []) do
    request = %{
      method: method,
      route: route,
      body: body,
      options: options
    }
    GenServer.call(Ratelimiter, {:queue, request, nil}, :infinity)
  end

  def handle_call({:queue, request, original_from}, from, state) do
    retry_time = request.route
      |> Bucket.get_ratelimit_timeout
    if retry_time do
      Task.start(fn -> wait_for_timeout(request, retry_time, original_from || from) end)
    else
      response = request.method
        |> Base.request(request.route, request.body, [], request.options)
        |> handle_ratelimit_headers(request.route)
        |> handle_global_ratelimit()
        |> format_response
      GenServer.reply(original_from || from, response)
    end

    {:noreply, state}
  end

  def wait_for_timeout(request, timeout, from) do
    Process.sleep(timeout + 1000)
    GenServer.call(Ratelimiter, {:queue, request, from}, :infinity)
  end

  defp handle_global_ratelimit(response) do
    case response do
      {:ok, %HTTPoison.Response{headers: headers}} ->
        global_limit = headers |> List.keyfind("X-RateLimit-Global", 0)
        global_limit = global_limit || false

        if global_limit do
          retry = headers |> List.keyfind("Retry-After", 0) |> value_from_rltuple |> String.to_integer
          Bucket.create_bucket("GLOBAL", 0, 0, Util.now() + retry)
        end
      _ ->
        response
    end

    response
  end

  defp handle_ratelimit_headers(response, route) do
    case response do
      {:ok, %HTTPoison.Response{headers: headers}} ->
        limit = headers |> List.keyfind("X-RateLimit-Limit", 0) |> value_from_rltuple
        remaining = headers |> List.keyfind("X-RateLimit-Remaining", 0) |> value_from_rltuple
        reset = headers |> List.keyfind("X-RateLimit-Reset", 0) |> value_from_rltuple

        if limit && remaining && reset do
          Bucket.create_bucket(route, limit, remaining, reset * 1000)
        end
      _ ->
        response
    end

    response
  end

  defp value_from_rltuple(tuple) when is_nil(tuple), do: nil
  defp value_from_rltuple(tuple) do
      tuple
      |> Tuple.to_list
      |> List.last
      |> String.to_integer
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