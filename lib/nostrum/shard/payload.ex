defmodule Nostrum.Shard.Payload do
  @moduledoc """
  Specifies maps to be used for WS payloads.
  """

  alias Nostrum.{Constants, Util}

  @large_threshold 250

  @doc false
  def heartbeat_payload(sequence) do
    sequence
    |> build_payload("HEARTBEAT")
  end

  @doc false
  def identity_payload(state) do
    {os, name} = :os.type
    %{
      "token" => state.token,
      "properties" => %{
        "$os" => Atom.to_string(os) <> " " <> Atom.to_string(name),
        "$browser" => "Nostrum",
        "$device" => "Nostrum",
        "$referrer" => "",
        "$referring_domain" => ""
      },
      "compress" => false,
      "large_threshold" => @large_threshold,
      "shard" => [state.shard_num, Util.num_shards]
    } |> build_payload("IDENTIFY")
  end

  @doc false
  def resume_payload(state) do
    %{
      "token" => state.token,
      "session_id" => state.session,
      "seq" => state.seq
    } |> build_payload("RESUME")
  end

  @doc false
  def status_update_payload(idle_since, game, stream, status, afk, type) do
    %{
      "since" => idle_since,
      "afk" => afk,
      "status" => status,
      "game" => %{
        "name" => game,
        "type" => type,
        "url" => stream
      }
    } |> build_payload("STATUS_UPDATE")
  end

  @doc false
  def request_members_payload(guild_id, limit) do
    %{
      "guild_id" => guild_id,
      "query" => "",
      "limit" => limit
    } |> build_payload("REQUEST_GUILD_MEMBERS")
  end

  defp build_payload(data, opcode_name) do
    opcode = Constants.opcode_from_name(opcode_name)
    %{"op" => opcode, "d" => data} |> :erlang.term_to_binary
  end

end
