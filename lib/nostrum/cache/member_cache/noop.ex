defmodule Nostrum.Cache.MemberCache.NoOp do
  @moduledoc """
  A NoOp implementation for the MemberCache

  This cache does nothing, enable it if you dont need to cache members
  """
  @moduledoc since: "0.9.0"
  @behaviour Nostrum.Cache.MemberCache

  alias Nostrum.Cache.MemberCache
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Util
  use Supervisor

  @doc "Start the supervisor."
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl Supervisor
  def init(_init_arg) do
    Supervisor.init([], strategy: :one_for_one)
  end

  @impl MemberCache
  def by_user(_user_id), do: []

  @impl MemberCache
  def by_guild(_guild_id), do: []

  @impl MemberCache
  def get(_guild_id, _user_id), do: {:error, :member_not_found}

  @impl MemberCache
  def create(_guild_id, payload), do: Util.cast(payload, {:struct, Member})

  @impl MemberCache
  def update(guild_id, payload), do: {guild_id, nil, Util.cast(payload, {:struct, Member})}

  @impl MemberCache
  def delete(_guild_id, _user_id), do: :noop

  @impl MemberCache
  def bulk_create(_guild_id, _members), do: true
end
