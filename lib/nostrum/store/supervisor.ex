defmodule Nostrum.Store.Supervisor do
  @moduledoc """
  Supervises processes managing nostrum's internal state.

  Please see the following modules for more details:
  - `Nostrum.Store.GuildShardMapping`
  - `Nostrum.Store.UnavailableGuild`
  """
  @moduledoc since: "0.8.0"

  use Supervisor

  def start_link(%{name: _name} = bot_options) do
    Supervisor.start_link(__MODULE__, bot_options)
  end

  def init(%{name: name} = _bot_options) do
    children = [
      {Nostrum.Store.GuildShardMapping, name: name},
      {Nostrum.Store.UnavailableGuild, name: name}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
