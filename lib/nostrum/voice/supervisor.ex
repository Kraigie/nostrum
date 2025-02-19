defmodule Nostrum.Voice.Supervisor do
  @moduledoc false

  use Supervisor

  alias Nostrum.Struct.VoiceState
  alias Nostrum.Voice.Session

  require Logger

  def start_link(bot_options) do
    Supervisor.start_link(__MODULE__, bot_options, name: __MODULE__)
  end

  def init(bot_options) do
    children = [
      {DynamicSupervisor, strategy: :one_for_one, name: Nostrum.Voice.SessionSupervisor},
      {Nostrum.Voice, bot_options}
    ]

    options = [
      strategy: :one_for_one
    ]

    Supervisor.init(children, options)
  end

  def create_session(%VoiceState{} = voice, %{consumer: _consumer} = bot_options) do
    child = %{
      id: voice.guild_id,
      start: {Session, :start_link, [voice, bot_options]},
      restart: :transient
    }

    {:ok, _pid} = DynamicSupervisor.start_child(Nostrum.Voice.SessionSupervisor, child)
  end
end
