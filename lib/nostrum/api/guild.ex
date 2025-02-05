defmodule Nostrum.Api.Guild do
  @moduledoc """
  Functions for interacting with the Discord API's guild endpoints.

  See: https://discord.com/developers/docs/resources/guild
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.AuditLog
  alias Nostrum.Struct.Guild.AuditLogEntry
  alias Nostrum.Struct.Guild.Ban
  alias Nostrum.Struct.Guild.Integration
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.Guild.Role
  alias Nostrum.Struct.Guild.ScheduledEvent
  alias Nostrum.Struct.User
  alias Nostrum.Struct.VoiceRegion
  alias Nostrum.Struct.Webhook

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
  @spec add_member(Guild.id(), User.id(), Api.options()) ::
          Api.error() | {:ok, Member.t()} | {:ok}
  def add_member(guild_id, user_id, options)

  def add_member(guild_id, user_id, options) when is_list(options),
    do: add_member(guild_id, user_id, Map.new(options))

  def add_member(guild_id, user_id, %{} = options)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    Api.request(:put, Constants.guild_member(guild_id, user_id), options)
    |> Helpers.handle_request_with_decode({:struct, Member})
  end

  @doc """
  Adds a role to a member.

  Role to add is specified by `role_id`.
  User to add role to is specified by `guild_id` and `user_id`.
  An optional `reason` can be given for the audit log.
  """
  @spec add_member_role(Guild.id(), User.id(), Role.id(), AuditLogEntry.reason()) ::
          Api.error() | {:ok}
  def add_member_role(guild_id, user_id, role_id, reason \\ nil) do
    Api.request(%{
      method: :put,
      route: Constants.guild_member_role(guild_id, user_id, role_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc """
  Begins a guild prune to prune members within `days`.

  An optional `reason` can be provided for the guild audit log.

  This endpoint requires the `KICK_MEMBERS` permission. It fires multiple
  `t:Nostrum.Consumer.guild_member_remove/0` events.

  If successful, returns `{:ok, %{pruned: pruned}}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.begin_prune(81384788765712384, 1)
  {:ok, %{pruned: 0}}
  ```
  """
  @spec begin_prune(Guild.id(), 1..30, AuditLogEntry.reason()) ::
          Api.error() | {:ok, %{pruned: integer}}
  def begin_prune(guild_id, days, reason \\ nil)
      when is_snowflake(guild_id) and days in 1..30 do
    %{
      method: :post,
      route: Constants.guild_prune(guild_id),
      body: "",
      params: [days: days],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Bans a user from a guild.

  User to delete is specified by `guild_id` and `user_id`.
  An optional `reason` can be specified for the audit log.
  """
  @spec ban_member(Guild.id(), User.id(), integer, AuditLogEntry.reason()) ::
          Api.error() | {:ok}
  def ban_member(guild_id, user_id, days_to_delete, reason \\ nil) do
    Api.request(%{
      method: :put,
      route: Constants.guild_ban(guild_id, user_id),
      body: %{delete_message_days: days_to_delete},
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc ~S"""
  Creates a new emoji for the given guild.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  An optional `reason` can be provided for the audit log.

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the emoji
    * `:image` (base64 data URI) - the 128x128 emoji image. Maximum size of 256kb
    * `:roles` (list of `t:Nostrum.Snowflake.t/0`) - roles for which this emoji will be whitelisted
    (default: [])

  `:name` and `:image` are always required.

  ## Examples

  ```elixir
  image = "data:image/png;base64,YXl5IGJieSB1IGx1a2luIDQgc3VtIGZ1az8="

  Nostrum.Api.Guild.create_emoji(43189401384091, name: "nostrum", image: image, roles: [])
  ```
  """
  @spec create_emoji(Guild.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Emoji.t()}
  def create_emoji(guild_id, options, reason \\ nil)

  def create_emoji(guild_id, options, reason) when is_list(options),
    do: create_emoji(guild_id, Map.new(options), reason)

  def create_emoji(guild_id, %{} = options, reason) do
    %{
      method: :post,
      route: Constants.guild_emojis(guild_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Emoji})
  end

  @doc """
  Creates a new guild integeration.

  Guild to create integration with is specified by `guild_id`.

  `options` is a map with the following requires keys:
   * `type` - Integration type.
   * `id` - Integeration id.
  """
  @spec create_integration(integer, %{
          type: String.t(),
          id: integer
        }) :: Api.error() | {:ok}
  def create_integration(guild_id, options) do
    Api.request(:post, Constants.guild_integrations(guild_id), options)
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
  Nostrum.Api.Guild.create_role(41771983423143937, name: "nostrum-club", hoist: true)
  ```
  """
  @spec create_role(Guild.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Role.t()}
  def create_role(guild_id, options, reason \\ nil)

  def create_role(guild_id, options, reason) when is_list(options),
    do: create_role(guild_id, Map.new(options), reason)

  def create_role(guild_id, %{} = options, reason) when is_snowflake(guild_id) do
    %{
      method: :post,
      route: Constants.guild_roles(guild_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Role})
  end

  @doc ~S"""
  Deletes a guild.

  This endpoint requires that the current user is the owner of the guild.
  It fires the `t:Nostrum.Consumer.guild_delete/0` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.delete(81384788765712384)
  {:ok}
  ```
  """
  @spec delete(Guild.id()) :: Api.error() | {:ok}
  def delete(guild_id) when is_snowflake(guild_id) do
    Api.request(:delete, Constants.guild(guild_id))
  end

  @doc ~S"""
  Deletes the given emoji.

  An optional `reason` can be provided for the audit log.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  If successful, returns `{:ok}`. Otherwise, returns `t:Nostrum.Api.error/0`.
  """
  @spec delete_emoji(Guild.id(), Emoji.id(), AuditLogEntry.reason()) ::
          Api.error() | {:ok}
  def delete_emoji(guild_id, emoji_id, reason \\ nil) do
    Api.request(%{
      method: :delete,
      route: Constants.guild_emoji(guild_id, emoji_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc """
  Deletes a guild integeration.

  Integration to delete is specified by `guild_id` and `integeration_id`.
  """
  @spec delete_integration(Guild.id(), integer) :: Api.error() | {:ok}
  def delete_integration(guild_id, integration_id) do
    Api.request(:delete, Constants.guild_integration(guild_id, integration_id))
  end

  @doc ~S"""
  Deletes a role from a guild.

  An optional `reason` can be specified for the audit log.

  This endpoint requires the `MANAGE_ROLES` permission. It fires a
  `t:Nostrum.Consumer.guild_role_delete/0` event.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.delete_role(41771983423143937, 392817238471936)
  ```
  """
  @spec delete_role(Guild.id(), Role.id(), AuditLogEntry.reason()) :: Api.error() | {:ok}
  def delete_role(guild_id, role_id, reason \\ nil)
      when is_snowflake(guild_id) and is_snowflake(role_id) do
    Api.request(%{
      method: :delete,
      route: Constants.guild_role(guild_id, role_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc ~S"""
  Gets a guild.

  If successful, returns `{:ok, guild}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.get(81384788765712384)
  {:ok, %Nostrum.Struct.Guild{id: 81384788765712384}}
  ```
  """
  @spec get(Guild.id()) :: Api.error() | {:ok, Guild.rest_guild()}
  def get(guild_id) when is_snowflake(guild_id) do
    Api.request(:get, Constants.guild(guild_id))
    |> Helpers.handle_request_with_decode({:struct, Guild})
  end

  @doc ~S"""
  Get the `t:Nostrum.Struct.Guild.AuditLog.t/0` for the given `guild_id`.

  ## Options

    * `:user_id` (`t:Nostrum.Struct.User.id/0`) - filter the log for a user ID
    * `:action_type` (`t:integer/0`) - filter the log by audit log type, see [Audit Log Events](https://discord.com/developers/docs/resources/audit-log#audit-log-entry-object-audit-log-events)
    * `:before` (`t:Nostrum.Struct.Snowflake.t/0`) - filter the log before a certain entry ID
    * `:limit` (`t:pos_integer/0`) - how many entries are returned (default 50, minimum 1, maximum 100)
  """
  @spec audit_log(Guild.id(), Api.options()) :: {:ok, AuditLog.t()} | Api.error()
  def audit_log(guild_id, options \\ []) do
    Api.request(:get, Constants.guild_audit_logs(guild_id), "", options)
    |> Helpers.handle_request_with_decode({:struct, AuditLog})
  end

  @doc """
  Gets a ban object for the given user from a guild.
  """
  @doc since: "1.x.x"
  @spec ban(Guild.id(), User.id()) :: Api.error() | {:ok, Ban.t()}
  def ban(guild_id, user_id) do
    Api.request(:get, Constants.guild_ban(guild_id, user_id))
    |> Helpers.handle_request_with_decode({:struct, Ban})
  end

  @doc """
  Gets a list of users banned from a guild.

  Guild to get bans for is specified by `guild_id`.
  """
  @spec bans(Guild.id()) :: Api.error() | {:ok, [User.t()]}
  def bans(guild_id) do
    Api.request(:get, Constants.guild_bans(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, User}})
  end

  @doc ~S"""
  Gets a list of guild channels.

  If successful, returns `{:ok, channels}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.channels(81384788765712384)
  {:ok, [%Nostrum.Struct.Channel{guild_id: 81384788765712384} | _]}
  ```
  """
  @spec channels(Guild.id()) :: Api.error() | {:ok, [Channel.guild_channel()]}
  def channels(guild_id) when is_snowflake(guild_id) do
    Api.request(:get, Constants.guild_channels(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, Channel}})
  end

  @doc ~S"""
  Gets an emoji for the given guild and emoji ids.

  This endpoint requires the `MANAGE_EMOJIS` permission.

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.
  """
  @spec emoji(Guild.id(), Emoji.id()) :: Api.error() | {:ok, Emoji.t()}
  def emoji(guild_id, emoji_id) do
    Api.request(:get, Constants.guild_emoji(guild_id, emoji_id))
    |> Helpers.handle_request_with_decode({:struct, Emoji})
  end

  @doc """
  Gets a list of guild integerations.

  Guild to get integrations for is specified by `guild_id`.
  """
  @spec integrations(Guild.id()) ::
          Api.error() | {:ok, [Integration.t()]}
  def integrations(guild_id) do
    Api.request(:get, Constants.guild_integrations(guild_id))
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Gets a guild member.

  If successful, returns `{:ok, member}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.member(4019283754613, 184937267485)
  ```
  """
  @spec member(Guild.id(), User.id()) :: Api.error() | {:ok, Member.t()}
  def member(guild_id, user_id) when is_snowflake(guild_id) and is_snowflake(user_id) do
    Api.request(:get, Constants.guild_member(guild_id, user_id))
    |> Helpers.handle_request_with_decode({:struct, Member})
  end

  @doc """
  Gets the number of members that would be removed in a prune given `days`.

  This endpoint requires the `KICK_MEMBERS` permission.

  If successful, returns `{:ok, %{pruned: pruned}}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.estimate_prune_count(81384788765712384, 1)
  {:ok, %{pruned: 0}}
  ```
  """
  @spec estimate_prune_count(Guild.id(), 1..30) :: Api.error() | {:ok, %{pruned: integer}}
  def estimate_prune_count(guild_id, days) when is_snowflake(guild_id) and days in 1..30 do
    Api.request(:get, Constants.guild_prune(guild_id), "", days: days)
    |> Helpers.handle_request_with_decode()
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
  Nostrum.Api.Guild.modify_role(41771983423143937, 392817238471936, hoist: false, name: "foo-bar")
  ```
  """
  @spec modify_role(Guild.id(), Role.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Role.t()}
  def modify_role(guild_id, role_id, options, reason \\ nil)

  def modify_role(guild_id, role_id, options, reason) when is_list(options),
    do: modify_role(guild_id, role_id, Map.new(options), reason)

  def modify_role(guild_id, role_id, %{} = options, reason)
      when is_snowflake(guild_id) and is_snowflake(role_id) do
    %{
      method: :patch,
      route: Constants.guild_role(guild_id, role_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
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
  @spec remove_member_role(Guild.id(), User.id(), Role.id(), AuditLogEntry.reason()) ::
          Api.error() | {:ok}
  def remove_member_role(guild_id, user_id, role_id, reason \\ nil) do
    Api.request(%{
      method: :delete,
      route: Constants.guild_member_role(guild_id, user_id, role_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc ~S"""
  Gets a guild's roles.

  If successful, returns `{:ok, roles}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.roles(147362948571673)
  ```
  """
  @spec roles(Guild.id()) :: Api.error() | {:ok, [Role.t()]}
  def roles(guild_id) when is_snowflake(guild_id) do
    Api.request(:get, Constants.guild_roles(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, Role}})
  end

  @doc """
  Get a list of scheduled events for a guild.
  """
  @doc since: "1.x.x"
  @spec scheduled_events(Guild.id()) :: Api.error() | {:ok, [ScheduledEvent.t()]}
  def scheduled_events(guild_id) do
    Api.request(:get, Constants.guild_scheduled_events(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, ScheduledEvent}})
  end

  @doc """
  Gets a list of webhooks for a guild.

  ## Parameters
    - `guild_id` - Guild to get webhooks for.
  """
  @spec webhooks(Guild.id()) :: Api.error() | {:ok, [Webhook.t()]}
  def webhooks(guild_id) do
    Api.request(:get, Constants.webhooks_guild(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, Webhook}})
  end

  @doc """
  Gets a guild embed.
  """
  @spec widget(Guild.id()) :: Api.error() | {:ok, map}
  def widget(guild_id) do
    Api.request(:get, Constants.guild_widget(guild_id))
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Gets a list of voice regions for the guild.

  Guild to get voice regions for is specified by `guild_id`.
  """
  @spec voice_region(Guild.id()) :: Api.error() | {:ok, [VoiceRegion.t()]}
  def voice_region(guild_id) do
    Api.request(:get, Constants.guild_voice_regions(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, VoiceRegion}})
  end

  @doc """
  Leaves a guild.

  Guild to leave is specified by `guild_id`.
  """
  @spec leave(Guild.id()) :: Api.error() | {:ok}
  def leave(guild_id) do
    Api.request(%{
      method: :delete,
      route: Constants.me_guild(guild_id),
      body: "",
      params: [],
      headers: []
    })
  end

  @doc ~S"""
  Gets a list of emojis for a given guild.

  This endpoint requires the `MANAGE_EMOJIS` permission.

  If successful, returns `{:ok, emojis}`. Otherwise, returns `t:Nostrum.Api.error/0`.
  """
  @spec emojis(Guild.id()) :: Api.error() | {:ok, [Emoji.t()]}
  def emojis(guild_id) do
    Api.request(:get, Constants.guild_emojis(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, Emoji}})
  end

  @doc """
  Gets a list of a guild's members.

  If successful, returns `{:ok, members}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:limit` (integer) - max number of members to return (1-1000) (default: 1)
    * `:after` (`t:Nostrum.Struct.User.id/0`) - the highest user id in the previous page (default: 0)

  ## Examples

  ```elixir
  Nostrum.Api.Guild.members(41771983423143937, limit: 1)
  ```
  """
  @spec members(Guild.id(), Api.options()) :: Api.error() | {:ok, [Member.t()]}
  def members(guild_id, options \\ %{})

  def members(guild_id, options) when is_list(options),
    do: members(guild_id, Map.new(options))

  def members(guild_id, %{} = options) when is_snowflake(guild_id) do
    Api.request(:get, Constants.guild_members(guild_id), "", options)
    |> Helpers.handle_request_with_decode({:list, {:struct, Member}})
  end

  @doc """
  Gets a list of voice regions.
  """
  @spec voice_regions() :: Api.error() | {:ok, [VoiceRegion.t()]}
  def voice_regions do
    Api.request(:get, Constants.regions())
    |> Helpers.handle_request_with_decode({:list, {:struct, VoiceRegion}})
  end

  @doc """
  Modifies the nickname of the current user in a guild.

  If successful, returns `{:ok, %{nick: nick}}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:nick` (string) - value to set users nickname to

  ## Examples

  ```elixir
  Nostrum.Api.Guild.modify_self_nick(41771983423143937, nick: "Nostrum")
  {:ok, %{nick: "Nostrum"}}
  ```
  """
  @spec modify_self_nick(Guild.id(), Api.options()) :: Api.error() | {:ok, %{nick: String.t()}}
  def modify_self_nick(guild_id, options \\ %{}) do
    Api.request(:patch, Constants.guild_me_nick(guild_id), options)
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Modifies a guild's settings.

  This endpoint requires the `MANAGE_GUILD` permission. It fires the
  `t:Nostrum.Consumer.guild_update/0` event.

  An optional `reason` can be provided for the audit log.

  If successful, returns `{:ok, guild}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - guild name
    * `:region` (string) - guild voice region id
    * `:verification_level` (integer) - verification level
    * `:default_message_notifications` (integer) - default message
    notification level
    * `:explicit_content_filter` (integer) - explicit content filter level
    * `:afk_channel_id` (`t:Nostrum.Snowflake.t/0`) - id for afk channel
    * `:afk_timeout` (integer) - afk timeout in seconds
    * `:icon` (base64 data URI) - 128x128 jpeg image for the guild icon
    * `:owner_id` (`t:Nostrum.Snowflake.t/0`) - user id to transfer
    guild ownership to (must be owner)
    * `:splash` (base64 data URI) - 128x128 jpeg image for the guild splash
    (VIP only)
    * `:system_channel_id` (`t:Nostrum.Snowflake.t/0`) - the id of the
    channel to which system messages are sent
    * `:rules_channel_id` (`t:Nostrum.Snowflake.t/0`) - the id of the channel that
    is used for rules in public guilds
    * `:public_updates_channel_id` (`t:Nostrum.Snowflake.t/0`) - the id of the channel
    where admins and moderators receive notices from Discord in public guilds

  ## Examples

  ```elixir
  Nostrum.Api.Guild.modify(451824027976073216, name: "Nose Drum")
  {:ok, %Nostrum.Struct.Guild{id: 451824027976073216, name: "Nose Drum", ...}}
  ```
  """
  @spec modify(Guild.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Guild.rest_guild()}
  def modify(guild_id, options \\ [], reason \\ nil)

  def modify(guild_id, options, reason) when is_list(options),
    do: modify(guild_id, Map.new(options), reason)

  def modify(guild_id, options, reason) when is_snowflake(guild_id) and is_map(options) do
    %{
      method: :patch,
      route: Constants.guild(guild_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Guild})
  end

  @doc """
  Reorders a guild's channels.

  This endpoint requires the `MANAGE_CHANNELS` permission. It fires multiple
  `t:Nostrum.Consumer.channel_update/0` events.

  If successful, returns `{:ok, channels}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  `positions` is a list of maps that each map a channel id with a position.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.modify_channel_positions(279093381723062272, [%{id: 351500354581692420, position: 2}])
  {:ok}
  ```
  """
  @spec modify_channel_positions(Guild.id(), [%{id: Channel.id(), position: integer}]) ::
          Api.error() | {:ok}
  def modify_channel_positions(guild_id, positions)
      when is_snowflake(guild_id) and is_list(positions) do
    Api.request(:patch, Constants.guild_channels(guild_id), positions)
  end

  @doc ~S"""
  Modify the given emoji.

  This endpoint requires the `MANAGE_EMOJIS` permission. It fires a
  `t:Nostrum.Consumer.guild_emojis_update/0` event.

  An optional `reason` can be provided for the audit log.

  If successful, returns `{:ok, emoji}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - name of the emoji
    * `:roles` (list of `t:Nostrum.Snowflake.t/0`) - roles to which this emoji will be whitelisted

  ## Examples

  ```elixir
  Nostrum.Api.Guild.modify_emoji(43189401384091, 4314301984301, name: "elixir", roles: [])
  ```
  """
  @spec modify_emoji(Guild.id(), Emoji.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Emoji.t()}
  def modify_emoji(guild_id, emoji_id, options \\ %{}, reason \\ nil)

  def modify_emoji(guild_id, emoji_id, options, reason) when is_list(options),
    do: modify_emoji(guild_id, emoji_id, Map.new(options), reason)

  def modify_emoji(guild_id, emoji_id, options, reason) when is_map(options) do
    %{
      method: :patch,
      route: Constants.guild_emoji(guild_id, emoji_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Emoji})
  end

  @doc """
  Changes the settings and behaviours for a guild integeration.

  Integration to modify is specified by `guild_id` and `integeration_id`.

  `options` is a map with the following keys:
   * `expire_behavior` - Expiry behavior.
   * `expire_grace_period` - Period where the integration will ignore elapsed subs.
   * `enable_emoticons` - Whether emoticons should be synced.
  """
  @spec modify_integration(Guild.id(), Integration.id(), %{
          expire_behaviour: integer,
          expire_grace_period: integer,
          enable_emoticons: boolean
        }) :: Api.error() | {:ok}
  def modify_integration(guild_id, integration_id, options) do
    Api.request(:patch, Constants.guild_integration(guild_id, integration_id), options)
  end

  @doc ~S"""
  Modifies a guild member's attributes.

  This endpoint fires the `t:Nostrum.Consumer.guild_member_update/0` event.
  It situationally requires the `MANAGE_NICKNAMES`, `MANAGE_ROLES`,
  `MUTE_MEMBERS`, `DEAFEN_MEMBERS`, and `MOVE_MEMBERS` permissions.

  If successful, returns `{:ok, member}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  An optional `reason` argument can be given for the audit log.

  ## Options

    * `:nick` (string) - value to set users nickname to
    * `:roles` (list of `t:Nostrum.Snowflake.t/0`) - array of role ids the member is assigned
    * `:mute` (boolean) - if the user is muted
    * `:deaf` (boolean) - if the user is deafened
    * `:channel_id` (`t:Nostrum.Snowflake.t/0`) - id of channel to move user to (if they are connected to voice)
    * `:communication_disabled_until` (`t:DateTime.t/0` or `nil`) - datetime to disable user communication (timeout) until, or `nil` to remove timeout.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.modify_member(41771983423143937, 637162356451, nick: "Nostrum")
  {:ok, %Nostrum.Struct.Member{}}
  ```
  """
  @spec modify_member(Guild.id(), User.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Member.t()}
  def modify_member(guild_id, user_id, options \\ %{}, reason \\ nil)

  def modify_member(guild_id, user_id, options, reason) when is_list(options),
    do: modify_member(guild_id, user_id, Map.new(options), reason)

  def modify_member(guild_id, user_id, %{} = options, reason)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    options =
      options
      |> Helpers.maybe_convert_date_time(:communication_disabled_until)

    Api.request(%{
      method: :patch,
      route: Constants.guild_member(guild_id, user_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
    |> Helpers.handle_request_with_decode({:struct, Member})
  end

  @doc ~S"""
  Reorders a guild's roles.

  This endpoint requires the `MANAGE_ROLES` permission. It fires multiple
  `t:Nostrum.Consumer.guild_role_update/0` events.

  If successful, returns `{:ok, roles}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  `positions` is a list of maps that each map a role id with a position.

  ## Examples

  ```elixir
  Nostrum.Api.Guild.modify_role_positions(41771983423143937, [%{id: 41771983423143936, position: 2}])
  ```
  """
  @spec modify_role_positions(
          Guild.id(),
          [%{id: Role.id(), position: integer}],
          AuditLogEntry.reason()
        ) :: Api.error() | {:ok, [Role.t()]}
  def modify_role_positions(guild_id, positions, reason \\ nil)
      when is_snowflake(guild_id) and is_list(positions) do
    %{
      method: :patch,
      route: Constants.guild_roles(guild_id),
      body: positions,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:list, {:struct, Role}})
  end

  @doc """
  Modifies a guild embed.
  """
  @spec modify_widget(Guild.id(), map) :: Api.error() | {:ok, map}
  def modify_widget(guild_id, options) do
    Api.request(:patch, Constants.guild_widget(guild_id), options)
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Removes a ban for a user.

  User to unban is specified by `guild_id` and `user_id`.
  An optional `reason` can be specified for the audit log.
  """
  @spec unban_member(Guild.id(), User.id(), AuditLogEntry.reason()) :: Api.error() | {:ok}
  def unban_member(guild_id, user_id, reason \\ nil) do
    Api.request(%{
      method: :delete,
      route: Constants.guild_ban(guild_id, user_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc """
  Removes a member from a guild.

  This event requires the `KICK_MEMBERS` permission. It fires a
  `t:Nostrum.Consumer.guild_member_remove/0` event.

  An optional reason can be provided for the audit log with `reason`.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  iex> Nostrum.Api.Guild.kick_member(1453827904102291, 18739485766253)
  {:ok}
  ```
  """
  @spec kick_member(Guild.id(), User.id(), AuditLogEntry.reason()) :: Api.error() | {:ok}
  def kick_member(guild_id, user_id, reason \\ nil)
      when is_snowflake(guild_id) and is_snowflake(user_id) do
    Api.request(%{
      method: :delete,
      route: Constants.guild_member(guild_id, user_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc """
  Syncs a guild integration.

  Integration to sync is specified by `guild_id` and `integeration_id`.
  """
  @spec sync_integration(Guild.id(), Integration.id()) :: Api.error() | {:ok}
  def sync_integration(guild_id, integration_id) do
    Api.request(:post, Constants.guild_integration_sync(guild_id, integration_id))
  end
end
