defmodule Nostrum.Api.Guild do
  alias Nostrum.Api
  alias Nostrum.Constants
  alias Nostrum.Struct
  alias Nostrum.Struct.User
  alias Nostrum.Struct.Guild.Member

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc ~S"""
  Puts a user in a guild.

  This endpoint fires the `t:Nostrum.Consumer.guild_member_add/0` event.
  It requires the `CREATE_INSTANT_INVITE` permission. Additionally, it
  situationally requires the `MANAGE_NICKNAMES`, `MANAGE_ROLES`,
  `MUTE_MEMBERS`, and `DEAFEN_MEMBERS` permissions.

  If successful, returns `{:ok, member}` or `{:ok}` if the user was already a member of the
  guild. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:access_token` (string) - the user's oauth2 access token
    * `:nick` (string) - value to set users nickname to
    * `:roles` (list of `t:Nostrum.Struct.Guild.Role.id/0`) - array of role ids the member is assigned
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened

  `:access_token` is always required.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.add_member(
    41771983423143937,
    18374719829378473,
    access_token: "6qrZcUqja7812RVdnEKjpzOL4CvHBFG",
    nick: "nostrum",
    roles: [431849301, 913809431]
  )
  ```
  """
  @spec add_member(Struct.Guild.id(), User.id(), Api.options()) ::
          Api.error() | {:ok, Member.t()} | {:ok}
  def add_member(guild_id, user_id, options)

  def add_member(guild_id, user_id, options) when is_list(options),
    do: add_member(guild_id, user_id, Map.new(options))

  def add_member(guild_id, user_id, %{} = options)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    Api.request(:put, Constants.guild_member(guild_id, user_id), options)
    |> Api.handle_request_with_decode({:struct, Member})
  end
end
