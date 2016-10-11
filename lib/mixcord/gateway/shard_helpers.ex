defmodule Mixcord.Shard.Helpers do
  @moduledoc """
  """

  alias Mixcord.Constants
  alias Mixcord.Rest.Client
  require Logger

  def status_update(pid, {idle, game}) do
    status_json = Poison.encode!(%{game: %{name: game}})
    send(pid, {:status_update, status_json})
  end

  @doc false
  def handle_event(payload, state_map) do
    #state_map.caller.handle_event({event, payload}, other, stuff) to run users commands
    case payload.t do
      :READY ->
        :noop
      :GUILD_CREATE ->
        Mixcord.Caches.Guilds.create!(payload.d)
      :MESSAGE_CREATE ->
        :noop
      _ ->
        :noop
    end
  end

  @doc false
  def heartbeat_payload(sequence) do
    build_payload(Constants.opcode_from_name("HEARTBEAT"), sequence)
  end

  @doc false
  def identity_payload(state_map) do
    data = %{
      "token" => state_map.token,
      "properties" => %{
        "$os" => "Linux",
        "$browser" => "Mixcord",
        "$device" => "Mixcord",
        "$referrer" => "",
        "$referring_domain" => ""
      },
      "compress" => false,
      "large_threshold" => 250
    }
    build_payload(Constants.opcode_from_name("IDENTIFY"), data)
  end

  @doc false
  def status_update_payload(json) do
    build_payload(Constants.opcode_from_name("STATUS_UPDATE"), json)
  end

  @doc false
  def build_payload(opcode, data, event \\ nil, seq \\ nil) do
    %{"op" => opcode, "d" => data, "s" => seq, "t" => event}
      |> :erlang.term_to_binary
  end

  @doc false
  def heartbeat(pid, interval) do
    Process.send_after(pid, {:heartbeat, interval}, interval)
  end

  @doc false
  def identify(pid) do
    send(pid, :identify)
  end

  @doc false
  def gateway do
    if Enum.member?(:ets.all, :gateway_url) do
      url = :ets.lookup(:gateway_url, "url")
        |> List.first
      url["url"]
    else
      :ets.new(:gateway_url, [:named_table])
      new_url = get_new_gateway_url()
      :ets.insert(:gateway_url, {"url", new_url})
      new_url
    end
  end

  defp get_new_gateway_url do
    case Client.request(:get, Constants.gateway, "") do
      {:error, status_code: status_code, message: message} ->
        raise(Mixcord.Errors.ApiError, status_code: status_code, message: message)
      {:ok, body: body} ->
        body = Poison.decode!(body)
        gateway_url = body["url"] <> "?encoding=etf&v=6"
          |> to_charlist
    end
  end

end