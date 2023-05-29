defmodule Nostrum.Store.Supervisor do
  @moduledoc """
  Supervises processes managing nostrum's internal state.

  Please see the following modules for more details:
  - `Nostrum.Store.GuildShardMapping`
  - `Nostrum.Store.UnavailableGuild`
  """
  @moduledoc since: "0.8.0"

  use Supervisor

  def start_link([]) do
    Supervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def init([]) do
    children = [
      Nostrum.Store.GuildShardMapping,
      Nostrum.Store.UnavailableGuild
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
