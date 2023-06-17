defmodule Nostrum.Cache.MemberCache.NoOp do
  @moduledoc """
  A NoOp implementation for the MemberCache

  This cache does nothing, enable it if you dont need to cache members
  """
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
  def create(_guild_id, payload), do: Util.cast(payload, {:struct, Member})

  @impl MemberCache
  def update(guild_id, payload), do: {guild_id, nil, Util.cast(payload, {:struct, Member})}

  @impl MemberCache
  def delete(_guild_id, _user_id), do: :noop

  @impl MemberCache
  def bulk_create(_guild_id, _members), do: true

  @impl MemberCache
  def query_handle, do: :qlc.string_to_handle('[].')
end
