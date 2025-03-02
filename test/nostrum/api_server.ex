defmodule :nostrum_test_api_server do
  @moduledoc """
  Exports a simple Discord API-like server for testing API requests.
  """

  require Logger
  use GenServer

  ## normal exports

  def remaining_calls(bucket) do
    GenServer.call(__MODULE__, {:check_limits, bucket})
  end

  def reset_bucket(bucket) do
    :ok = GenServer.call(__MODULE__, {:reset_bucket, bucket})
  end

  ## mod_esi API

  defp decode_opts([]), do: %{}
  defp decode_opts(query), do: URI.decode_query(to_string(query))

  def accounted(session, _env, input) do
    opts = decode_opts(input)
    bucket_name = Map.get(opts, "bucket_name", "accounted")
    remaining = remaining_calls(bucket_name)
    Logger.debug("Remaining calls on bucket #{bucket_name}: #{remaining}")

    response =
      if remaining < 0 do
        """
        status: 429 Too Many Requests\r
        Content-Type: application/json\r
        x-ratelimit-remaining: 0\r
        x-ratelimit-reset-after: 0.5\r
        \r
        {"YOU HAVE BEEN BANNED": "FOREVER!"}
        """
      else
        """
        status: 200 OK\r
        Content-Type: application/json\r
        x-ratelimit-remaining: #{remaining}\r
        x-ratelimit-reset-after: 0.5\r
        \r
        {"request": "received"}
        """
      end

    :ok =
      :mod_esi.deliver(
        session,
        to_charlist(response)
      )
  end

  def endpoint(session, _env, _input) do
    :ok =
      :mod_esi.deliver(
        session,
        ~c"""
        status: 200 OK\r
        Content-Type: application/json\r
        x-ratelimit-remaining: 1\r
        x-ratelimit-reset-after: 0.2\r
        \r
        {"request": "received"}
        """
      )
  end

  def global_limit(session, _env, _input) do
    :ok =
      :mod_esi.deliver(
        session,
        ~c"""
        status: 200 OK\r
        Content-Type: application/json\r
        x-ratelimit-scope: global\r
        x-ratelimit-remaining: 0\r
        x-ratelimit-reset-after: 0.2\r
        \r
        {"request": "received"}
        """
      )
  end

  def user_limit(session, _env, _input) do
    :ok =
      :mod_esi.deliver(
        session,
        ~c"""
        status: 200 OK\r
        Content-Type: application/json\r
        x-ratelimit-scope: user\r
        x-ratelimit-remaining: 0\r
        x-ratelimit-reset-after: 0.2\r
        \r
        {"request": "received"}
        """
      )
  end

  def empty_body(session, _env, _input) do
    :ok =
      :mod_esi.deliver(
        session,
        ~c"""
        status: 204 No Content\r
        x-ratelimit-remaining: 50\r
        x-ratelimit-reset-after: 0.2\r
        \r
        """
      )
  end

  def maybe_crash(session, _env, input) do
    opts = decode_opts(input)
    crash = Map.get(opts, "crash", "no")
    remaining = String.to_integer(Map.get(opts, "remaining", "1"))

    response =
      if crash == "yes" do
        """
        status: 502 Bad Gateway\r
        Content-Type: text/html\r
        \r
        <noscript>i am k1p the psycho</noscript>
        """
      else
        """
        status: 200 OK\r
        Content-Type: application/json\r
        x-ratelimit-remaining: #{remaining}\r
        x-ratelimit-reset-after: 0.5\r
        \r
        {"request": "received"}
        """
      end

    :ok = :mod_esi.deliver(session, to_charlist(response))
  end

  def slowpoke(session, _env, input) do
    opts = decode_opts(input)
    duration = String.to_integer(Map.get(opts, "duration", "1000"))
    Process.sleep(duration)

    response = """
    status: 200 OK\r
    Content-Type: application/json\r
    x-ratelimit-remaining: 5\r
    x-ratelimit-reset-after: 0.5\r
    \r
    {"request": "received"}
    """

    :ok = :mod_esi.deliver(session, to_charlist(response))
  end

  ## GenServer API

  def child_spec(buckets) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [buckets]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end

  def start_link(buckets) do
    GenServer.start_link(__MODULE__, buckets, name: __MODULE__)
  end

  @impl true
  def init(buckets) do
    # Needed for cleanup.
    Process.flag(:trap_exit, true)
    {:ok, buckets}
  end

  @impl true
  def handle_call({:check_limits, bucket}, _from, buckets) do
    case Map.get(buckets, bucket) do
      %{total: count, remaining: count, reset_after: interval, timer: nil} = match ->
        Logger.debug("Starting expiration timer for bucket #{bucket}")
        {:ok, timer} = :timer.apply_after(interval, __MODULE__, :reset_bucket, [bucket])
        remaining = count - 1
        state = Map.put(buckets, bucket, %{match | remaining: remaining, timer: timer})
        {:reply, remaining, state}

      %{remaining: count, reset_after: _interval} = match ->
        Logger.debug("Bucket #{bucket} has #{count - 1} call(s) remaining")
        remaining = count - 1
        state = Map.put(buckets, bucket, %{match | remaining: remaining})
        {:reply, remaining, state}

      nil ->
        Logger.debug("Do not have any ratelimit information for bucket #{bucket}")
        {:reply, nil, buckets}
    end
  end

  def handle_call({:reset_bucket, bucket}, _from, buckets) when is_map_key(buckets, bucket) do
    match = Map.fetch!(buckets, bucket)
    buckets = Map.put(buckets, bucket, %{match | remaining: match.total, timer: nil})
    {:reply, :ok, buckets}
  end

  def handle_call({:reset_bucket, _bucket}, _from, buckets) do
    {:reply, :unknown_bucket, buckets}
  end
end
