defmodule Mixcord.Shard.Payload do
  @moduledoc false

  alias Mixcord.Constants
  alias Mixcord.Util

  def state_map(token, caller, shard_num) do
    %{
      token: token,
      caller: caller,
      shard_num: shard_num,
      seq: nil,
      session: nil,
      reconnect_attempts: 0,
      last_heartbeat: 0,
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
  def status_update_payload(json) do
    build_payload(Constants.opcode_from_name("STATUS_UPDATE"), json)
  end

  @doc false
  def build_payload(opcode, data, event \\ nil, seq \\ nil) do
    %{"op" => opcode, "d" => data, "s" => seq, "t" => event}
      |> :erlang.term_to_binary
  end

end