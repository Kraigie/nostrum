defmodule Nostrum.Cache.GuildCache do
  @moduledoc """
  Functions for retrieving guild states.
  """

  alias Nostrum.Cache.Guild.GuildServer
  alias Nostrum.Cache.Mapping.ChannelGuild
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Message
  alias Nostrum.Util

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @type clause ::
          {:id, Guild.id()}
          | {:channel_id, Channel.id()}
          | {:message, Message.t()}

  @type clauses :: [clause] | map

  @type selector :: (Guild.t() -> any)

  @type reason ::
          :id_not_found
          | :id_not_found_on_guild_lookup
          | :channel_not_found

  defguardp is_selector(term) when is_function(term, 1)

  @doc """
  Retrives all `Nostrum.Struct.Guild` from the cache as a stream.
  """
  @spec all() :: Enum.t()
  def all, do: select_all(fn guild -> guild end)

  @doc """
  Selects values using a `selector` from all `Nostrum.Struct.Guild` in the cache.
  """
  @spec select_all(selector) :: Enum.t()
  def select_all(selector)

  def select_all(selector) when is_selector(selector) do
    Supervisor.which_children(GuildSupervisor)
    |> Stream.map(fn {_, pid, _, _} -> pid end)
    |> Task.async_stream(fn pid -> GenServer.call(pid, {:select, selector}) end)
    |> Stream.map(fn {:ok, value} -> value end)
  end

  @doc """
  Retrives a single `Nostrum.Struct.Guild` from the cache via its `id`.

  Returns `{:error, reason}` if no result was found.

  ## Examples

  ```Elixir
  iex> Nostrum.Cache.GuildCache.get(0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get(10)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec get(Guild.id()) :: {:ok, Guild.t()} | {:error, reason}
  def get(id) do
    select(id, fn guild -> guild end)
  end

  @doc ~S"""
  Same as `get/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get!(Guild.id()) :: Guild.t() | no_return
  def get!(id), do: get(id) |> Util.bangify_find(id, __MODULE__)

  @doc """
  Retrives a single `Nostrum.Struct.Guild` where it matches the `clauses`.

  Returns `{:error, reason}` if no result was found.

  ```Elixir
  iex> Nostrum.Cache.GuildCache.get_by(id: 0)
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(%{id: 0})
  {:ok, %Nostrum.Struct.Guild{id: 0}}

  iex> Nostrum.Cache.GuildCache.get_by(id: 10)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec get_by(clauses) :: {:ok, Guild.t()} | {:error, reason}
  def get_by(clauses) do
    select_by(clauses, fn guild -> guild end)
  end

  @doc ~S"""
  Same as `get_by/1`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec get_by!(clauses) :: Guild.t() | no_return
  def get_by!(clauses), do: get_by(clauses) |> Util.bangify_find(clauses, __MODULE__)

  @doc """
  Selects values using a `selector` from a `Nostrum.Struct.Guild`.

  Returns `{:error, reason}` if no result was found.

  ## Examples

  ```Elixir
  iex> Nostrum.Cache.GuildCache.select(0, fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select(10, fn guild -> guild.id end)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec select(Guild.id(), selector) :: {:ok, any} | {:error, reason}
  def select(id, selector) do
    select_by(%{id: id}, selector)
  end

  @doc ~S"""
  Same as `select/2`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec select!(Guild.id(), selector) :: any | no_return
  def select!(id, selector), do: select(id, selector) |> Util.bangify_find(id, __MODULE__)

  @doc """
  Selects values using a `selector` from a `Nostrum.Struct.Guild` that matches
  the `clauses`.

  Returns `{:error, reason}` if no result was found.

  ```Elixir
  iex> Nostrum.Cache.GuildCache.select_by([id: 0], fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select_by(%{id: 0}, fn guild -> guild.id end)
  {:ok, 0}

  iex> Nostrum.Cache.GuildCache.select_by([id: 10], fn guild -> guild.id end)
  {:error, :id_not_found_on_guild_lookup}
  ```
  """
  @spec select_by(clauses, selector) :: {:ok, any} | {:error, reason}
  def select_by(clauses, selector)

  def select_by(clauses, selector) when is_list(clauses) and is_selector(selector),
    do: select_by(Map.new(clauses), selector)

  def select_by(%{id: id}, selector) when is_snowflake(id) and is_selector(selector) do
    case GuildServer.select(id, selector) do
      {:error, _} = error -> error
      guild -> {:ok, guild}
    end
  end

  def select_by(%{channel_id: channel_id}, selector)
      when is_snowflake(channel_id) and is_selector(selector) do
    case ChannelGuild.get_guild(channel_id) do
      {:ok, guild_id} -> select_by(%{id: guild_id}, selector)
      {:error, _} = error -> error
    end
  end

  def select_by(%{message: %Message{channel_id: channel_id}}, selector) do
    select_by(%{channel_id: channel_id}, selector)
  end

  @doc ~S"""
  Same as `select_by/2`, but raises `Nostrum.Error.CacheError` in case of failure.
  """
  @spec select_by!(clauses, selector) :: any | no_return
  def select_by!(clauses, selector),
    do: select_by(clauses, selector) |> Util.bangify_find(clauses, __MODULE__)
end
