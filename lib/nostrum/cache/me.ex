defmodule Nostrum.Cache.Me do
  @moduledoc """
  Simple cache that stores information for the current user.
  """

  use Agent

  alias Nostrum.Struct.User

  def start_link([]) do
    Agent.start_link(fn -> nil end, name: __MODULE__)
  end

  @doc ~S"""
  Returns the current user's state.
  """
  @spec get() :: User.t() | nil
  def get do
    Agent.get(__MODULE__, fn user -> user end)
  end

  @doc false
  @spec put(User.t()) :: :ok
  def put(%User{} = user) do
    Agent.update(__MODULE__, fn _ -> user end)
  end

  @doc false
  @spec update(map) :: :ok
  def update(%{} = values) do
    Agent.update(__MODULE__, fn state ->
      struct(state, values)
    end)
  end

  @doc false
  @spec delete() :: :ok
  def delete do
    Agent.update(__MODULE__, fn _ -> nil end)
  end
end
