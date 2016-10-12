defmodule Mixcord do
  @moduledoc """
  """

  def start_link(token, shard, blah) do
    Mixcord.Cache.Supervisor.start_link
    Mixcord.Shard.Supervisor.start_link("", "ME", 1)
  end

end
