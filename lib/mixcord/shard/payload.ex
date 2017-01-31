defmodule Mixcord.Shard.Payload do
  @moduledoc false

  alias Mixcord.Constants
  alias Mixcord.Util

  @large_threshold 250

  def state_map(token, caller, shard_num, pid) do
    %{
      token: token,
      caller: caller,
      shard_num: shard_num,
      seq: nil,
      session: nil,
      reconnect_attempts: 0,
      last_heartbeat: 0,
      shard_pid: pid,
      heartbeat_intervals: Enum.map(1..10, fn _ -> 0 end)
    }
  end

  @doc false
  def heartbeat_payload(sequence) do
    build_payload(Constants.opcode_from_name("HEARTBEAT"), sequence)
  end

  @doc false
  def identity_payload(state) do
    data = %{
      "token" => state.token,
      "properties" => %{
        "$os" => "Linux",
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
  def status_update_payload(idle_since, game) do
    data = %{
      "idle_since": idle_since,
      "game": %{
        "name": game.name
      }
    }
    build_payload(Constants.opcode_from_name("STATUS_UPDATE"), data)
  end

  @doc false
  def request_members_payload(guild_id, limit) do
    data = %{
      "guild_id": guild_id,
      "query": "",
      "limit": limit
    }
    build_payload(Constants.opcode_from_name("REQUEST_GUILD_MEMBERS"), data)
  end

  @doc false
  def build_payload(opcode, data) do
    %{"op" => opcode, "d" => data}
      |> :erlang.term_to_binary
  end

end