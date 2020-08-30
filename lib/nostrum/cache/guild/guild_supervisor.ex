defmodule Nostrum.Cache.Guild.GuildSupervisor do
  @moduledoc false

  use DynamicSupervisor

  alias Nostrum.Cache.Guild.GuildServer

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def start_child(id, guild) do
    spec = {GuildServer, id: id, guild: guild}
    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  def init([]) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
