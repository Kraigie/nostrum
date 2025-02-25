defmodule Nostrum.Cache.Me do
  @moduledoc """
  Simple cache that stores information for the current user.
  """

  use Agent

  alias Nostrum.Bot
  alias Nostrum.Struct.User
  alias Nostrum.Util

  def start_link([]) do
    Agent.start_link(fn -> nil end)
  end

  @doc ~S"""
  Returns the current user's state.
  """
  @spec get() :: User.t() | nil
  def get do
    Agent.get(fetch_me(), fn user -> user end)
  end

  @doc false
  @spec put(User.t()) :: :ok
  def put(%User{} = user) do
    Agent.update(fetch_me(), fn _ -> user end)
  end

  @doc false
  @spec update(map) :: :ok
  def update(%{} = values) do
    Agent.update(fetch_me(), fn state ->
      struct(state, values)
    end)
  end

  @doc false
  @spec delete() :: :ok
  def delete do
    Agent.update(fetch_me(), fn _ -> nil end)
  end

  @doc false
  @spec fetch_me() :: Agent.agent()
  def fetch_me do
    Bot.fetch_bot_pid()
    |> Util.get_child_pid(Nostrum.Cache.Supervisor)
    |> Util.get_child_pid(Nostrum.Cache.Me)
  end
end
