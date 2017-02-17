defmodule Mixcord.Shard.Payload do
  @moduledoc """
  Specifies maps to be used for WS payloads.. and also the state map.
  """

  alias Mixcord.{Constants, Util}

  @large_threshold 250

  @typedoc """
  The state map contained and maintained in each websocket process.

  Keys
   * `token` - The token of the bot.
   * `shard_num` - The shard number container this state.
   * `seq` - Current seq number of the websocket.
   * `session` - Current session of the websocket.
   * `reconnect_attempts` - Current number of reconnect attempts.
   * `last_heartbeat` - The time of the last heartbeat.
   * `shard_pid` - Pid of the shard containing this state.
   * `producer_pid` - Pid of the producer attached to this shard
   * `heartbeat_intervals` - List of last ten heartbeat intervals, from hearbeat send to ack.
  """
  @type state_map :: Map.t

  def state_map(token, shard_num, pid) do
    %{
      token: token,
      shard_num: shard_num,
      seq: nil,
      session: nil,
      reconnect_attempts: 0,
      last_heartbeat: 0,
      shard_pid: pid,
      producer_pid: nil,
      heartbeat_intervals: Enum.map(1..10, fn _ -> 0 end)
    }
  end

  @doc false
  def heartbeat_payload(sequence) do
    build_payload(Constants.opcode_from_name("HEARTBEAT"), sequence)
  end

  @doc false
  def identity_payload(state) do
    {os, name} = :os.type
    data = %{
      "token" => state.token,
      "properties" => %{
        "$os" => Atom.to_string(os) <> " " <> Atom.to_string(name),
        "$browser" => "Mixcord",
        "$device" => "Mixcord",
        "$referrer" => "",
        "$referring_domain" => ""
      },
      "compress" => false,
      "large_threshold" => @large_threshold,
      "shard" => [state.shard_num, Util.num_shards]
    }
    build_payload(Constants.opcode_from_name("IDENTIFY"), data)
  end

  @doc false
  def resume_payload(state) do
    data = %{
      "token" => state.token,
      "session_id" => state.session,
      "seq" => state.seq
    }
    build_payload(Constants.opcode_from_name("RESUME"), data)
  end

  @doc false
  def status_update_payload(idle_since, game, status, afk) do
    data = %{
      "since" => idle_since,
      "afk" => afk,
      "status" => status,
      "game" => %{
        "name" => game
      }
    }
    build_payload(Constants.opcode_from_name("STATUS_UPDATE"), data)
  end

  @doc false
  def request_members_payload(guild_id, limit) do
    data = %{
      "guild_id" => guild_id,
      "query" => "",
      "limit" => limit
    }
    build_payload(Constants.opcode_from_name("REQUEST_GUILD_MEMBERS"), data)
  end

  @doc false
  def build_payload(opcode, data) do
    %{"op" => opcode, "d" => data}
      |> :erlang.term_to_binary
  end

end
