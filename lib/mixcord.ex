defmodule Mixcord do
  @moduledoc """
  """

  use Application

  def start(_, _) do
    import Supervisor.Spec

    token = Application.get_env(:mixcord, :token)
    caller = Application.get_env(:mixcord, :caller)
    num_shards = Application.get_env(:mixcord, :num_shards)

    children = [
      worker(Mixcord.Rest.Ratelimiter, []),
      supervisor(Mixcord.Cache.Supervisor, []),
      supervisor(Mixcord.Shard.Supervisor, [token, caller, num_shards])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def handle_event(_, _) do
    
  end

end
