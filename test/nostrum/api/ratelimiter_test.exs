defmodule Nostrum.Api.RatelimiterTest do
  alias Nostrum.Api.Ratelimiter
  use ExUnit.Case, async: true

  @moduletag :capture_log

  @request_timeout :timer.seconds(5)

  setup do
    host = ~c"localhost"

    {:ok, server} =
      :inets.start(:httpd,
        port: 0,
        bind_address: host,
        modules: [:mod_esi],
        server_name: ~c"kraigie cat",
        server_root: ~c"/tmp",
        document_root: ~c"/tmp",
        erl_script_alias: {~c"/api/v10", [:nostrum_test_api_server]}
      )

    info = :httpd.info(server)
    port = :proplists.get_value(:port, info)
    {:ok, _started} = :application.ensure_all_started(:gun)
    _ratelimiter_group = start_supervised!(Nostrum.Api.RatelimiterGroup)
    options = %{host: host, port: port, wrapped_token: fn -> "token" end}
    spec = {Nostrum.Api.Ratelimiter, {options, []}}
    ratelimiter = start_supervised!(spec)
    [ratelimiter: ratelimiter, server: server]
  end

  defp build_request(function, query_params \\ []) do
    %{
      method: :get,
      route: "/nostrum_test_api_server:#{function}",
      body: "",
      headers: [{"X-Craigie-Cat", "Hungry"}],
      params: query_params
    }
  end

  describe "basic requests" do
    test "work", %{ratelimiter: ratelimiter} do
      request = build_request("endpoint")
      # This _should_ trigger some of the queueing behaviour due to
      # `X-Ratelimit-Remaining: 0` on that endpoint.
      for _ <- 1..3 do
        reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
        assert {:ok, body} = reply
        assert %{"request" => "received"} = Jason.decode!(body)
      end
    end

    test "work with 204 responses", %{ratelimiter: ratelimiter} do
      request = build_request("empty_body")
      reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
      assert {:ok} = reply
    end
  end

  describe "accounted requests" do
    setup do
      server =
        start_supervised!(
          {:nostrum_test_api_server,
           %{"accounted" => %{remaining: 2, reset_after: 500, timer: nil, total: 2}}}
        )

      [accountant: server]
    end

    # These would cause the GenServer that checks the ratelimits to crash
    test "do not run into the request limit", %{ratelimiter: ratelimiter} do
      request = build_request("accounted")

      for _ <- 1..3 do
        reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
        assert {:ok, body} = reply
        assert %{"request" => "received"} = Jason.decode!(body)
      end
    end

    test "do not run into the request limit with parallel requests", %{ratelimiter: ratelimiter} do
      request = build_request("accounted")

      parent = self()

      for _ <- 1..3 do
        spawn(fn ->
          reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
          assert {:ok, body} = reply
          assert %{"request" => "received"} = Jason.decode!(body)
          send(parent, :ok)
        end)
      end

      # XXX: This takes longer than it should. Why?
      assert_receive :ok, :timer.seconds(5)
      assert_receive :ok, :timer.seconds(5)
      assert_receive :ok, :timer.seconds(5)
    end
  end

  describe "accounted requests that start out with a 429" do
    setup do
      bucket_name = "thoughtcrime"
      bucket = %{remaining: 0, reset_after: 500, timer: nil, total: 2}
      buckets = %{bucket_name => bucket}
      server = start_supervised!({:nostrum_test_api_server, buckets})

      # The server normally starts this on its own when remaining drops to 0.
      # But because we start out with 0, we gotta start the reset manually.
      {:ok, _timer} =
        :timer.apply_after(bucket.reset_after, :nostrum_test_api_server, :reset_bucket, [
          bucket_name
        ])

      [accountant: server, bucket_name: bucket_name]
    end

    # These would cause the GenServer that checks the ratelimits to crash
    test "wait their turn", %{bucket_name: bucket_name, ratelimiter: ratelimiter} do
      request = build_request("accounted", bucket_name: bucket_name)

      for _ <- 1..3 do
        reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
        assert {:ok, body} = reply
        assert %{"request" => "received"} = Jason.decode!(body)
      end
    end
  end

  describe "user limit" do
    @tag skip: "expected fail, needs requeue after leaving limit state"
    test "ceases to send requests", %{ratelimiter: ratelimiter} do
      request = build_request("user_limit")
      reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
      # TODO: needs monotime measurement to check if time to next request > 200ms
      assert {:ok, body} = reply
      assert %{"request" => "received"} = Jason.decode!(body)
      me = self()

      spawn(fn ->
        reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
        send(me, reply)
      end)

      refute_receive _, 200
      assert_receive {:ok, _body}, 200
    end
  end

  # XXX: can use parametrize and async on elixir & exunit 1.18+ for this and
  # the above (and probably more)
  describe "global limit" do
    @tag skip: "expected fail, needs requeue after leaving limit state"
    test "ceases to send requests", %{ratelimiter: ratelimiter} do
      request = build_request("global_limit")
      _reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
      me = self()

      spawn(fn ->
        reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
        send(me, reply)
      end)

      refute_receive _, 200
      assert_receive {:ok, _body}, 200
    end
  end
end

defmodule :nostrum_test_api_server do
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
    remaining = remaining_calls(Map.get(opts, "bucket_name", "accounted"))

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
      %{remaining: count, reset_after: interval, timer: nil} = match ->
        {:ok, timer} = :timer.apply_after(interval, __MODULE__, :reset_bucket, [bucket])
        remaining = count - 1
        state = Map.put(buckets, bucket, %{match | remaining: remaining, timer: timer})
        {:reply, remaining, state}

      %{remaining: count, reset_after: _interval, timer: timer} = match ->
        remaining = count - 1
        state = Map.put(buckets, bucket, %{match | remaining: remaining, timer: timer})
        {:reply, remaining, state}

      nil ->
        {:reply, nil, buckets}
    end
  end

  def handle_call({:reset_bucket, bucket}, _from, buckets) do
    match = Map.fetch!(buckets, bucket)
    buckets = Map.put(buckets, bucket, %{match | remaining: match.total, timer: nil})
    {:reply, :ok, buckets}
  end

  @impl true
  def terminate(_reason, buckets) do
    buckets
    |> Stream.filter(fn {_key, %{timer: timer}} -> not is_nil(timer) end)
    |> Enum.each(fn {_key, %{timer: timer}} -> {:ok, :cancel} = :timer.cancel(timer) end)
  end
end
