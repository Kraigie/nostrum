defmodule Nostrum.Voice.Supervisor do
  @moduledoc false

  use Supervisor

  alias Nostrum.Struct.VoiceState
  alias Nostrum.Voice.Session

  require Logger

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, [], name: VoiceSupervisor)
  end

  def init(_opts) do
    children = [
      Nostrum.Voice
    ]

    options = [
      strategy: :one_for_one
    ]

    Supervisor.init(children, options)
  end

  def create_session(%VoiceState{} = voice) do
    child = %{
      id: voice.guild_id,
      start: {Session, :start_link, [voice]}
    }

    Supervisor.start_child(VoiceSupervisor, child)
  end

  def end_session(guild_id) do
    VoiceSupervisor |> Supervisor.terminate_child(guild_id)
    VoiceSupervisor |> Supervisor.delete_child(guild_id)
  end
end
