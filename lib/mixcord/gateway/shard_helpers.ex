defmodule Mixcord.Shard.Helpers do
  @moduledoc """
  """

  alias Mixcord.Constants
  alias Mixcord.Rest.Client

  def status_update(pid, new_status) do
    send(pid, {:status_update, new_status})
  end

  def handle_event do

  end

  def identify do

  end

  @doc false
  def heartbeat(pid, interval) do
    Process.send_after(pid, {:heartbeat, interval}, interval)
  end

  @doc false
  def build_payload(opcode, data) do
    %{op: opcode, d: data}
      |> :erlang.term_to_binary
  end

  @doc false
  def gateway do
    if Enum.member?(:ets.all, :gateway_url) do
      IO.inspect "FOUND OLD GATEWAY"
      url = :ets.lookup(:gateway_url, "url")
        |> List.first
      url["url"]
    else
      IO.inspect "GETTING NEW GATEWAY"
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