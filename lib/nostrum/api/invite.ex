defmodule Nostrum.Api.Invite do
  @moduledoc """
  Functions for interacting with the Discord API's invite endpoints.

  See: https://discord.com/developers/docs/resources/invite
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.AuditLogEntry
  alias Nostrum.Struct.Invite

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc ~S"""
  Gets an invite by its `invite_code`.

  If successful, returns `{:ok, invite}`. Otherwise, returns a
  `t:Nostrum.Api.error/0`.

  ## Options

    * `:with_counts` (boolean) - whether to include member count fields

  ## Examples

  ```elixir
  Nostrum.Api.Invite.get("zsjUsC")

  Nostrum.Api.Invite.get("zsjUsC", with_counts: true)
  ```
  """
  @spec get(Invite.code(), Api.options()) :: Api.error() | {:ok, Invite.simple_invite()}
  def get(invite_code, options \\ []) when is_binary(invite_code) do
    Api.request(:get, Constants.invite(invite_code), "", options)
    |> Helpers.handle_request_with_decode({:struct, Invite})
  end

  @doc ~S"""
  Deletes an invite by its `invite_code`.

  This endpoint requires the `MANAGE_CHANNELS` permission.

  If successful, returns `{:ok, invite}`. Otherwise, returns a
  `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Invite.delete("zsjUsC")
  ```
  """
  @spec delete(Invite.code()) :: Api.error() | {:ok, Invite.simple_invite()}
  def delete(invite_code) when is_binary(invite_code) do
    Api.request(:delete, Constants.invite(invite_code))
    |> Helpers.handle_request_with_decode({:struct, Invite})
  end

  @doc ~S"""
  Gets a list of invites for a guild.

  This endpoint requires the `MANAGE_GUILD` permission.

  If successful, returns `{:ok, invites}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Invite.guild_invites(81384788765712384)
  {:ok, [%Nostrum.Struct.Invite{} | _]}
  ```
  """
  @spec guild_invites(Guild.id()) :: Api.error() | {:ok, [Invite.detailed_invite()]}
  def guild_invites(guild_id) when is_snowflake(guild_id) do
    Api.request(:get, Constants.guild_invites(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, Invite}})
  end

  @doc ~S"""
  Creates an invite for a guild channel.

  An optional `reason` can be provided for the audit log.

  This endpoint requires the `CREATE_INSTANT_INVITE` permission.

  If successful, returns `{:ok, invite}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:max_age` (integer) - duration of invite in seconds before expiry, or 0 for never.
      (default: `86400`)
    * `:max_uses` (integer) - max number of uses or 0 for unlimited.
      (default: `0`)
    * `:temporary` (boolean) - Whether the invite should grant temporary
      membership. (default: `false`)
    * `:unique` (boolean) - used when creating unique one time use invites.
      (default: `false`)

  ## Examples

  ```elixir
  Nostrum.Api.Invite.create(41771983423143933)
  {:ok, Nostrum.Struct.Invite{}}

  Nostrum.Api.Invite.create(41771983423143933, max_uses: 20)
  {:ok, %Nostrum.Struct.Invite{}}
  ```
  """
  @spec create(Channel.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Invite.detailed_invite()}
  def create(channel_id, options \\ [], reason \\ nil)

  def create(channel_id, options, reason) when is_list(options),
    do: create(channel_id, Map.new(options), reason)

  def create(channel_id, options, reason)
      when is_snowflake(channel_id) and is_map(options) do
    %{
      method: :post,
      route: Constants.channel_invites(channel_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Invite})
  end

  @doc ~S"""
  Gets a list of invites for a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'MANAGE_CHANNELS' permissions.

  If successful, returns `{:ok, invite}`. Otherwise, returns a
  `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  iex> Nostrum.Api.Invite.channel_invites(43189401384091)
  {:ok, [%Nostrum.Struct.Invite{} | _]}
  ```
  """
  @spec channel_invites(Channel.id()) :: Api.error() | {:ok, [Invite.detailed_invite()]}
  def channel_invites(channel_id) when is_snowflake(channel_id) do
    Api.request(:get, Constants.channel_invites(channel_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, Invite}})
  end
end
