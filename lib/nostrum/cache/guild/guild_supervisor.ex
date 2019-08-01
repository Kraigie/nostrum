defmodule Nostrum.Cache.Guild.GuildSupervisor do
  @moduledoc false

  use DynamicSupervisor

  alias Nostrum.Cache.Guild.GuildServer

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, [], name: GuildSupervisor)
  end

  def start_child(args) do
    DynamicSupervisor.start_child(__MODULE__, {GuildServer, args})
  end

  def init([]) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
