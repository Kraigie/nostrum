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

  defp run_request!(ratelimiter, type, query_params \\ []) do
    request = build_request(type, query_params)
    reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
    assert {:ok, body} = reply
    assert %{"request" => "received"} = Jason.decode!(body)
    reply
  end

  defp run_failing_request!(ratelimiter, type, expected_status, query_params) do
    request = build_request(type, query_params)
    reply = Ratelimiter.queue(ratelimiter, request, @request_timeout)
    assert {:error, %{status_code: ^expected_status}} = reply
    reply
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
      for _ <- 1..3 do
        run_request!(ratelimiter, "accounted")
      end
    end

    test "do not run into the request limit with parallel requests", %{ratelimiter: ratelimiter} do
      parent = self()

      for _ <- 1..3 do
        spawn(fn ->
          run_request!(ratelimiter, "accounted")
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

      # The server normally starts this on its own when the initial request is
      # sent. But because we start out with 0 in `remaining` instead of the 2
      # in `total`, we gotta start the reset manually.
      {:ok, timer} =
        :timer.apply_after(bucket.reset_after, :nostrum_test_api_server, :reset_bucket, [
          bucket_name
        ])

      on_exit(fn -> {:ok, :cancel} = :timer.cancel(timer) end)

      [accountant: server, bucket_name: bucket_name]
    end

    # These would cause the GenServer that checks the ratelimits to crash
    test "wait their turn", %{bucket_name: bucket_name, ratelimiter: ratelimiter} do
      for _ <- 1..3 do
        run_request!(ratelimiter, "accounted", bucket_name: bucket_name)
      end
    end
  end

  describe "user limit" do
    test "ceases to send requests", %{ratelimiter: ratelimiter} do
      run_request!(ratelimiter, "user_limit")
      me = self()

      spawn(fn ->
        reply = run_request!(ratelimiter, "user_limit")
        send(me, reply)
      end)

      refute_receive _, 200
      assert_receive {:ok, _body}, 200
    end
  end

  # XXX: can use parametrize and async on elixir & exunit 1.18+ for this and
  # the above (and probably more)
  describe "global limit" do
    test "ceases to send requests", %{ratelimiter: ratelimiter} do
      run_request!(ratelimiter, "global_limit")
      me = self()

      spawn(fn ->
        reply = run_request!(ratelimiter, "global_limit")
        send(me, reply)
      end)

      refute_receive _, 200
      assert_receive {:ok, _body}, 200
    end
  end

  describe "transient upstream errors" do
    test "won't stop future requests", %{ratelimiter: ratelimiter} do
      run_failing_request!(ratelimiter, "maybe_crash", 502, crash: "yes")
      run_request!(ratelimiter, "maybe_crash", crash: "no")
    end

    test "won't stop pipelining of future requests", %{ratelimiter: ratelimiter} do
      # X-Ratelimit-Remaining is "1" on that endpoint
      run_request!(ratelimiter, "maybe_crash", crash: "no", remaining: 2)
      run_failing_request!(ratelimiter, "maybe_crash", 502, crash: "yes")
      run_request!(ratelimiter, "maybe_crash", crash: "no")
    end
  end

  describe "hitting the bot call limit" do
    test "pauses requests temporarily if we can send the remaining requests in the next window",
         %{ratelimiter: ratelimiter} do
      request = build_request("maybe_crash", remaining: 60)

      reqids =
        Enum.reduce(1..52, :gen_statem.reqids_new(), fn n, acc ->
          :gen_statem.send_request(ratelimiter, {:queue, request}, n, acc)
        end)

      Enum.map(1..52, fn n ->
        result = :gen_statem.wait_response(reqids, @request_timeout, _delete = false)
        # `n` is just added here to help figure out which request failed
        assert {^n, {_response, _label, _new_collection}} = {n, result}
      end)
    end
  end
end
