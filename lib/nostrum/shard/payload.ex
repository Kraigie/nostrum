defmodule Nostrum.Shard.Payload do
  @moduledoc false

  alias Nostrum.{Constants, Shard.Intents}

  @large_threshold 250

  @doc false
  def heartbeat_payload(sequence) do
    sequence
    |> build_payload("HEARTBEAT")
  end

  @doc false
  def identity_payload(state) do
    {os, name} = :os.type()

    %{
      "token" => Application.get_env(:nostrum, :token),
      "properties" => %{
        "os" => Atom.to_string(os) <> " " <> Atom.to_string(name),
        "browser" => "Nostrum",
        "device" => "Nostrum"
      },
      "compress" => false,
      "large_threshold" => @large_threshold,
      "shard" => [state.shard_num, state.total_shards],
      "intents" => Intents.get_enabled_intents()
    }
    |> build_payload("IDENTIFY")
  end

  @doc false
  def resume_payload(state) do
    %{
      "token" => Application.get_env(:nostrum, :token),
      "session_id" => state.session,
      "seq" => state.seq
    }
    |> build_payload("RESUME")
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
    }
    |> build_payload("STATUS_UPDATE")
  end

  @doc false
  def update_voice_state_payload(guild_id, channel_id, self_mute, self_deaf) do
    %{
      "guild_id" => guild_id,
      "channel_id" => channel_id,
      "self_mute" => self_mute,
      "self_deaf" => self_deaf
    }
    |> build_payload("VOICE_STATUS_UPDATE")
  end

  @doc false
  def request_members_payload(guild_id, limit) do
    %{
      "guild_id" => guild_id,
      "query" => "",
      "limit" => limit
    }
    |> build_payload("REQUEST_GUILD_MEMBERS")
  end

  defp build_payload(data, opcode_name) do
    opcode = Constants.opcode_from_name(opcode_name)

    # term_to_iovec is the same as term_to_binary except it instead returns an iolist
    # a safe optimization since :gun.ws_send accepts an iolist
    %{"op" => opcode, "d" => data}
    |> :erlang.term_to_iovec()
  end
end
