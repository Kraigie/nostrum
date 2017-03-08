defmodule Nostrum.Cache.Me do
  @moduledoc """
  Simple cache that stores information for the current user.
  """

  use GenServer

  alias Nostrum.Struct.User

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: Me)
  end

  def init(_) do
    # Returns {:error, reason} if this fails, acting as a simple check for
    # correct tokens
    Nostrum.Api.get_current_user()
  end

  @doc """
  Retrieves the current user state.
  """
  @spec get() :: User.t | map
  def get do
    GenServer.call(Me, {:get})
  end

  def handle_call({:get}, _from, state) do
    {:reply, state, state}
  end
end
