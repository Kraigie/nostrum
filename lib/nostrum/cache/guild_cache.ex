defmodule Nostrum.Cache.GuildCache do
  @moduledoc """
  Functions for retrieving guild states.
  """

  alias Nostrum.Cache.Guild.GuildRegister
  alias Nostrum.Cache.Mapping.ChannelGuild
  alias Nostrum.Struct.Guild

  import Nostrum.Struct.Snowflake, only: [is_snowflake: 1]

  @type clauses :: Keyword.t() | map

  @type reason ::
          :invalid_clauses
          | :invalid_guild_id
          | :invalid_channel_id

  @doc """
  Retrives a single `Nostrum.Struct.Guild` from the cache via its `id`.

  Returns `{:error, reason}` if no result was found.

  ## Examples

  ```Elixir
  iex> Nostrum.Cache.GuildCache.get(0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get(10)
  {:error, :invalid_guild_id}
  ```
  """
  @spec get(Guild.id()) :: {:ok, Guild.t()} | {:error, reason}
  def get(id) when is_snowflake(id) do
    get_by(%{id: id})
  end

  @doc """
  Retrives a single `Nostrum.Struct.Guild` where it matches the `clauses`.

  Returns `{:error, reason}` if no result was found.

  ```Elixir
  iex> Nostrum.Cache.GuildCache.get_by(id: 0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(%{id: 0})
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(id: 10)
  {:error, :invalid_guild_id}
  ```
  """
  @spec get_by(clauses) :: {:ok, Guild.t()} | {:error, reason}
  def get_by(clauses)
  def get_by(clauses) when is_list(clauses), do: get_by(Map.new(clauses))

  def get_by(%{id: id}) when is_snowflake(id) do
    case GuildRegister.lookup(id) do
      {:ok, guild_pid} ->
        guild = GenServer.call(guild_pid, {:transform, fn state -> state end})
        {:ok, guild}

      {:error, _} ->
        {:error, :invalid_guild_id}
    end
  end

  # TODO: Possibly remove this. If channels store the guild_id with it, why should we need this?
  def get_by(%{channel_id: channel_id}) when is_snowflake(channel_id) do
    case ChannelGuild.get_guild(channel_id) do
      {:ok, guild_id} ->
        get_by(%{id: guild_id})

      {:error, _} ->
        {:error, :invalid_channel_id}
    end
  end

  def get_by(_), do: {:error, :invalid_clauses}
end
