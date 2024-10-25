defmodule Nostrum.Api.Role do
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.Guild.AuditLogEntry
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.User

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc """
  Adds a role to a member.

  Role to add is specified by `role_id`.
  User to add role to is specified by `guild_id` and `user_id`.
  An optional `reason` can be given for the audit log.
  """
  @spec add_member(Guild.id(), User.id(), Role.id(), AuditLogEntry.reason()) ::
          Api.error() | {:ok}
  def add_member(guild_id, user_id, role_id, reason \\ nil) do
    Api.request(%{
      method: :put,
      route: Constants.guild_member_role(guild_id, user_id, role_id),
      body: "",
      params: [],
      headers: Api.maybe_add_reason(reason)
    })
  end

  @doc ~S"""
  Creates a guild role.

  An optional reason for the audit log can be provided via `reason`.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_create/0` event.

  If successful, returns `{:ok, role}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the role (default: "new role")
    * `:permissions` (integer) - bitwise of the enabled/disabled permissions (default: @everyone perms)
    * `:color` (integer) - RGB color value (default: 0)
    * `:hoist` (boolean) - whether the role should be displayed separately in the sidebar (default: false)
    * `:mentionable` (boolean) - whether the role should be mentionable (default: false)
    * `:icon` (string) - URL role icon (default: `nil`)
    * `:unicode_emoji` (string) - standard unicode character emoji role icon (default: `nil`)

  ## Examples

  ```elixir
  Nostrum.Api.Role.create(41771983423143937, name: "nostrum-club", hoist: true)
  ```
  """
  @spec create(Guild.id(), Api.options(), AuditLogEntry.reason()) :: Api.error() | {:ok, Role.t()}
  def create(guild_id, options, reason \\ nil)

  def create(guild_id, options, reason) when is_list(options),
    do: create(guild_id, Map.new(options), reason)

  def create(guild_id, %{} = options, reason) when is_snowflake(guild_id) do
    %{
      method: :post,
      route: Constants.guild_roles(guild_id),
      body: options,
      params: [],
      headers: Api.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Role})
  end

  @doc ~S"""
  Deletes a role from a guild.

  An optional `reason` can be specified for the audit log.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_delete/0` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Role.delete(41771983423143937, 392817238471936)
  ```
  """
  @spec delete(Guild.id(), Role.id(), AuditLogEntry.reason()) :: Api.error() | {:ok}
  def delete(guild_id, role_id, reason \\ nil)
      when is_snowflake(guild_id) and is_snowflake(role_id) do
    Api.request(%{
      method: :delete,
      route: Constants.guild_role(guild_id, role_id),
      body: "",
      params: [],
      headers: Api.maybe_add_reason(reason)
    })
  end

  @doc ~S"""
  Modifies a guild role.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_update/0` event.

  An optional `reason` can be specified for the audit log.

  If successful, returns `{:ok, role}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the role
    * `:permissions` (integer) - bitwise of the enabled/disabled permissions
    * `:color` (integer) - RGB color value (default: 0)
    * `:hoist` (boolean) - whether the role should be displayed separately in the sidebar
    * `:mentionable` (boolean) - whether the role should be mentionable

  ## Examples

  ```elixir
  Nostrum.Api.Role.modify(41771983423143937, 392817238471936, hoist: false, name: "foo-bar")
  ```
  """
  @spec modify(Guild.id(), Role.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Role.t()}
  def modify(guild_id, role_id, options, reason \\ nil)

  def modify(guild_id, role_id, options, reason) when is_list(options),
    do: modify(guild_id, role_id, Map.new(options), reason)

  def modify(guild_id, role_id, %{} = options, reason)
      when is_snowflake(guild_id) and is_snowflake(role_id) do
    %{
      method: :patch,
      route: Constants.guild_role(guild_id, role_id),
      body: options,
      params: [],
      headers: Api.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Role})
  end

  @doc """
  Removes a role from a member.

  Role to remove is specified by `role_id`.
  User to remove role from is specified by `guild_id` and `user_id`.
  An optional `reason` can be given for the audit log.
  """
  @spec remove_member(Guild.id(), User.id(), Role.id(), AuditLogEntry.reason()) ::
          Api.error() | {:ok}
  def remove_member(guild_id, user_id, role_id, reason \\ nil) do
    Api.request(%{
      method: :delete,
      route: Constants.guild_member_role(guild_id, user_id, role_id),
      body: "",
      params: [],
      headers: Api.maybe_add_reason(reason)
    })
  end
end
