defmodule Nostrum.Cache.Me do
  @moduledoc """
  Simple cache that stores information for the current user.
  """

  use GenServer

  alias Nostrum.Api
  alias Nostrum.Struct.User

  def start_link([]) do
    GenServer.start_link(__MODULE__, [], name: Me)
  end

  def init([]) do
    # Returns {:error, reason} if this fails, acting as a simple check for
    # correct tokens
    with {:ok, user} <- Api.get_current_user(),
    do: {:ok, %{user | id: String.to_integer(user.id)}}
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
