defmodule Nostrum.Cache.PresenceCache.NoOp do
  @moduledoc """
  A cache module that does not cache anything.

  Useful for bots that do not need presence caching.
  """
  @moduledoc since: "0.5.0"
  @behaviour Nostrum.Cache.PresenceCache

  alias Nostrum.Cache.PresenceCache

  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.User

  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @doc "Start up the cache supervisor."
  @impl Supervisor
  def init(_init_arg) do
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl PresenceCache
  @spec get(Guild.id(), User.id()) :: {:error, :presence_not_found}
  def get(_guild_id, _user_id), do: {:error, :presence_not_found}

  @impl PresenceCache
  @spec create(map) :: :ok
  def create(_presence), do: :ok

  @impl PresenceCache
  @spec update(map) :: {Guild.id(), nil | map, map}
  def update(presence), do: {presence.guild_id, nil, presence}

  @impl PresenceCache
  @spec bulk_create(Guild.id(), [map]) :: :ok
  def bulk_create(_guild_id, _presences), do: :ok
end
