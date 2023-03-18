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
      {DynamicSupervisor, strategy: :one_for_one, name: VoiceSessionSupervisor},
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
      start: {Session, :start_link, [voice]},
      restart: :transient
    }

    DynamicSupervisor.start_child(VoiceSessionSupervisor, child)
  end
end
