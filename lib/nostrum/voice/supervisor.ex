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
    child = supervisor(
      Session,
      [voice],
      [id: voice.guild]
    )
    VoiceSupervisor |> Supervisor.start_child(child)
  end

  def end_session(guild) do
    VoiceSupervisor |> Supervisor.terminate_child(guild)
    VoiceSupervisor |> Supervisor.delete_child(guild)
  end

end
