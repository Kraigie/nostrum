defmodule Mixcord.Shard.Payload do
  @moduledoc false

  alias Mixcord.Constants
  alias Mixcord.Util

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
    build_payload(Constants.opcode_from_name("HEARTBEAT"), nil, sequence)
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
      "large_threshold" => 250,
      "shard" => [state_map.shard_num, Util.num_shards]
    }
    build_payload(Constants.opcode_from_name("IDENTIFY"), data)
  end

  @doc false
  def status_update_payload(idle_since, game) do
    data = %{
      "idle_since": idle_since,
      "game": %{
        "name": game.name
      }
    }
    build_payload(Constants.opcode_from_name("STATUS_UPDATE"), game)
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
  def build_payload(opcode, data, event \\ nil, seq \\ nil) do
    %{"op" => opcode, "d" => data, "s" => seq, "t" => event}
      |> :erlang.term_to_binary
  end

end