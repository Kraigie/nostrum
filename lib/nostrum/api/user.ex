defmodule Nostrum.Api.User do
  @moduledoc """
  Functions for interacting with the Discord API's user endpoints.

  See: https://discord.com/developers/docs/resources/user
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.User

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc ~S"""
  Create a new DM channel with a user.

  If successful, returns `{:ok, dm_channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.User.create_dm(150061853001777154)
  {:ok, %Nostrum.Struct.Channel{type: 1}}
  ```
  """
  @spec create_dm(User.id()) :: Api.error() | {:ok, Channel.dm_channel()}
  def create_dm(user_id) when is_snowflake(user_id) do
    Api.request(:post, Constants.me_channels(), %{recipient_id: user_id})
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Creates a new group DM channel.

  If successful, returns `{:ok, group_dm_channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  `access_tokens` are user oauth2 tokens. `nicks` is a map that maps a user id
  to a nickname.

  ## Examples

  ```elixir
  Nostrum.Api.User.create_group_dm(["6qrZcUqja7812RVdnEKjpzOL4CvHBFG"], %{41771983423143937 => "My Nickname"})
  {:ok, %Nostrum.Struct.Channel{type: 3}}
  ```
  """
  @spec create_group_dm([String.t()], %{optional(User.id()) => String.t()}) ::
          Api.error() | {:ok, Channel.group_dm_channel()}
  def create_group_dm(access_tokens, nicks) when is_list(access_tokens) and is_map(nicks) do
    Api.request(:post, Constants.me_channels(), %{access_tokens: access_tokens, nicks: nicks})
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Gets a user by its `t:Nostrum.Struct.User.id/0`.

  If the request is successful, this function returns `{:ok, user}`, where
  `user` is a `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get(User.id()) :: Api.error() | {:ok, User.t()}
  def get(user_id) do
    Api.request(:get, Constants.user(user_id))
    |> Helpers.handle_request_with_decode({:struct, User})
  end
end
