defmodule Nostrum.Cache.PresenceCache do
  @moduledoc """
  Cache for presences.

  The ETS table name associated with the User Cache is `:presences`. Besides the
  methods provided below you can call any other ETS methods on the table.

  ## Example
  ```elixir
  info = :ets.info(:presences)
  [..., heir: :none, name: :presences, size: 1, ...]
  size = info[:size]
  1
  ```
  """

  alias Nostrum.Struct.{Guild, User}
  alias Nostrum.Util

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc ~S"""
  Retreives a presence for a user from the cache by guild and id.

  If successful, returns `{:ok, presence}`. Otherwise returns `{:error, reason}`.

  ## Example
  ```elixir
  case Nostrum.Cache.PresenceCache.get(111133335555, 222244446666) do
    {:ok, presence} ->
      "They're #{presence.status}"
    {:error, _reason} ->
      "They're dead Jim"
  end
  ```
  """
  @spec get(User.id(), Guild.id()) :: {:error, :presence_not_found} | {:ok, map}
  def get(user_id, guild_id) when is_snowflake(user_id) and is_snowflake(guild_id) do
    case :ets.lookup(:presences, {user_id, guild_id}) do
      [] -> {:error, :presence_not_found}
      [{{^user_id, ^guild_id}, presence}] -> {:ok, presence}
    end
  end

  @doc """
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of a failure.
  """
  @spec get!(User.id(), Guild.id()) :: no_return | map
  def get!(user_id, guild_id) when is_snowflake(user_id) and is_snowflake(guild_id) do
    user_id |> get(guild_id) |> Util.bangify_find({user_id, guild_id}, __MODULE__)
  end

  @doc false
  @spec create(map) :: :ok
  def create(presence) do
    :ets.insert(:presences, {{presence.user.id, presence.guild_id}, presence})
    :ok
  end

  @doc false
  @spec update(map) :: {Guild.id(), nil | map, map} | :noop
  def update(presence) do
    case get(presence.user.id, presence.guild_id) do
      {:ok, p} ->
        new_presence = Map.merge(p, presence)
        create(new_presence)

        if p.activities == new_presence.activities and p.status == new_presence.status,
          do: :noop,
          else: {presence.guild_id, p, new_presence}

      {:error, _} ->
        create(presence)
        {presence.guild_id, nil, presence}
    end
  end

  @doc false
  @spec bulk_create(Guild.id(), [map]) :: :ok
  def bulk_create(_, []), do: :ok

  def bulk_create(guild_id, presences) when is_list(presences) do
    Enum.each(presences, fn p ->
      :ets.insert(:presences, {{p.user.id, guild_id}, p})
    end)
  end
end
