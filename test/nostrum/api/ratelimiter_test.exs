defmodule Nostrum.Api.RatelimiterTest do
  alias Nostrum.Api.Ratelimiter
  use ExUnit.Case, async: true

  @moduletag :capture_log

  setup_all do
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

  defp build_request(function, params \\ []) do
    %{
      method: :get,
      route: "/nostrum_test_api_server:#{function}?#{URI.encode_query(params)}",
      body: "",
      headers: [{"X-Craigie-Cat", "Hungry"}],
      params: []
    }
  end

  describe "basic requests" do
    test "work", %{ratelimiter: ratelimiter} do
      request = build_request("endpoint")
      # This _should_ trigger some of the queueing behaviour due to
      # `X-Ratelimit-Remaining: 0` on that endpoint.
      for _ <- 1..3 do
        reply = Ratelimiter.queue(ratelimiter, request)
        assert {:ok, body} = reply
        assert %{"request" => "received"} = :json.decode(body)
      end
    end
  end

  describe "user limit" do
    test "ceases to send requests", %{ratelimiter: ratelimiter} do
      request = build_request("user_limit")
      reply = Ratelimiter.queue(ratelimiter, request)
      assert {:ok, body} = reply
      assert %{"request" => "received"} = :json.decode(body)
    end
  end

  describe "global limit" do
    test "ceases to send requests", %{ratelimiter: ratelimiter} do
      request = build_request("global_limit")
      reply = Ratelimiter.queue(ratelimiter, request)
      assert {:ok, body} = reply
      assert %{"request" => "received"} = :json.decode(body)
    end
  end
end

defmodule :nostrum_test_api_server do
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

  def slowpoke(session, _env, _input) do
    Process.sleep(500)

    :ok =
      :mod_esi.deliver(
        session,
        ~c"""
        status: 200 OK\r
        Content-Type: application/json\r
        x-ratelimit-remaining: 1\r
        x-ratelimit-reset-after: 0.2\r
        \r
        {"napped": "a bit"}
        """
      )
  end
end
