defmodule Nostrum.Constants do
  @moduledoc false

  def domain, do: "discord.com"
  def base_route, do: "/api/v10"
  def base_url, do: "https://#{domain()}#{base_route()}"
  def cdn_url, do: "https://cdn.discordapp.com"
  def media_url, do: "https://media.discordapp.net"
  def gateway, do: "/gateway"
  def gateway_bot, do: "/gateway/bot"

  def channel(channel_id), do: "/channels/#{channel_id}"
  def channel_messages(channel_id), do: "/channels/#{channel_id}/messages"

  def channel_message(channel_id, message_id),
    do: "/channels/#{channel_id}/messages/#{message_id}"

  def channel_reaction_me(channel_id, message_id, emoji),
    do: "/channels/#{channel_id}/messages/#{message_id}/reactions/#{emoji}/@me"

  def channel_reaction(channel_id, message_id, emoji, user_id),
    do: "/channels/#{channel_id}/messages/#{message_id}/reactions/#{emoji}/#{user_id}"

  def channel_reactions_get(channel_id, message_id, emoji),
    do: "/channels/#{channel_id}/messages/#{message_id}/reactions/#{emoji}"

  def channel_reactions_delete(channel_id, message_id),
    do: "/channels/#{channel_id}/messages/#{message_id}/reactions"

  def channel_reactions_delete_emoji(channel_id, message_id, emoji),
    do: "/channels/#{channel_id}/messages/#{message_id}/reactions/#{emoji}"

  def channel_bulk_delete(channel_id), do: "/channels/#{channel_id}/messages/bulk-delete"

  def channel_permission(channel_id, overwrite_id),
    do: "/channels/#{channel_id}/permissions/#{overwrite_id}"

  def channel_invites(channel_id), do: "/channels/#{channel_id}/invites"
  def channel_typing(channel_id), do: "/channels/#{channel_id}/typing"
  def channel_pins(channel_id), do: "/channels/#{channel_id}/pins"
  def channel_pin(channel_id, message_id), do: "/channels/#{channel_id}/pins/#{message_id}"

  def guilds, do: "/guilds"
  def guild(guild_id), do: "/guilds/#{guild_id}"
  def guild_channels(guild_id), do: "/guilds/#{guild_id}/channels"
  def guild_members(guild_id), do: "/guilds/#{guild_id}/members"
  def guild_member(guild_id, user_id), do: "/guilds/#{guild_id}/members/#{user_id}"

  def guild_member_role(guild_id, user_id, role_id),
    do: "/guilds/#{guild_id}/members/#{user_id}/roles/#{role_id}"

  def guild_audit_logs(guild_id), do: "/guilds/#{guild_id}/audit-logs"
  def guild_bans(guild_id), do: "/guilds/#{guild_id}/bans"
  def guild_ban(guild_id, user_id), do: "/guilds/#{guild_id}/bans/#{user_id}"
  def guild_roles(guild_id), do: "/guilds/#{guild_id}/roles"
  def guild_role(guild_id, role_id), do: "/guilds/#{guild_id}/roles/#{role_id}"
  def guild_prune(guild_id), do: "/guilds/#{guild_id}/prune"
  def guild_voice_regions(guild_id), do: "/guilds/#{guild_id}/regions"
  def guild_invites(guild_id), do: "/guilds/#{guild_id}/invites"
  def guild_integrations(guild_id), do: "/guilds/#{guild_id}/integrations"

  def guild_integration(guild_id, integration_id),
    do: "/guilds/#{guild_id}/integrations/#{integration_id}"

  def guild_integration_sync(guild_id, integration_id),
    do: "/guilds/#{guild_id}/integrations/#{integration_id}/sync"

  def guild_emojis(guild_id), do: "/guilds/#{guild_id}/emojis"
  def guild_emoji(guild_id, emoji_id), do: "/guilds/#{guild_id}/emojis/#{emoji_id}"

  def sticker(sticker_id), do: "/stickers/#{sticker_id}"
  def guild_stickers(guild_id), do: "/guilds/#{guild_id}/stickers"
  def guild_sticker(guild_id, sticker_id), do: "/guilds/#{guild_id}/stickers/#{sticker_id}"

  def sticker_packs, do: "/sticker-packs"
  def sticker_pack(pack_id), do: "/sticker-packs/#{pack_id}"

  def guild_scheduled_events(guild_id), do: "/guilds/#{guild_id}/scheduled-events"

  def guild_scheduled_event(guild_id, event_id),
    do: "/guilds/#{guild_id}/scheduled-events/#{event_id}"

  def guild_scheduled_event_users(guild_id, event_id),
    do: "/guilds/#{guild_id}/scheduled-events/#{event_id}/users"

  def guild_widget(guild_id), do: "/guilds/#{guild_id}/widget"

  def webhooks_guild(guild_id), do: "/guilds/#{guild_id}/webhooks"
  def webhooks_channel(channel_id), do: "/channels/#{channel_id}/webhooks"
  def webhook(webhook_id), do: "/webhooks/#{webhook_id}"
  def webhook_token(webhook_id, webhook_token), do: "/webhooks/#{webhook_id}/#{webhook_token}"

  def webhook_message(webhook_id, webhook_token, message_id),
    do: "/webhooks/#{webhook_id}/#{webhook_token}/messages/#{message_id}"

  def webhook_message_edit(webhook_id, webhook_token, message_id),
    do: "/webhooks/#{webhook_id}/#{webhook_token}/messages/#{message_id}"

  def webhook_git(webhook_id, webhook_token),
    do: "/webhooks/#{webhook_id}/#{webhook_token}/github"

  def webhook_slack(webhook_id, webhook_token),
    do: "/webhooks/#{webhook_id}/#{webhook_token}/slack"

  def user(user_id), do: "/users/#{user_id}"

  def me, do: "/users/@me"
  def me_guilds, do: "/users/@me/guilds"
  def me_guild(guild_id), do: "/users/@me/guilds/#{guild_id}"
  def me_channels, do: "/users/@me/channels"
  def me_connections, do: "/users/@me/connections"

  def invite(invite_code), do: "/invites/#{invite_code}"
  def regions, do: "/voice/regions"

  def application_information, do: "/oauth2/applications/@me"

  def global_application_command(application_id, command_id),
    do: "/applications/#{application_id}/commands/#{command_id}"

  def global_application_commands(application_id), do: "/applications/#{application_id}/commands"

  def guild_application_command(application_id, guild_id, command_id),
    do: "/applications/#{application_id}/guilds/#{guild_id}/commands/#{command_id}"

  def guild_application_commands(application_id, guild_id),
    do: "/applications/#{application_id}/guilds/#{guild_id}/commands"

  def guild_application_command_permissions(application_id, guild_id),
    do: "/applications/#{application_id}/guilds/#{guild_id}/commands/permissions"

  def guild_application_command_permissions(application_id, guild_id, command_id),
    do: "/applications/#{application_id}/guilds/#{guild_id}/commands/#{command_id}/permissions"

  def original_interaction_response(application_id, interaction_token),
    do: "/webhooks/#{application_id}/#{interaction_token}/messages/@original"

  def interaction_callback(interaction_id, interaction_token),
    do: "/interactions/#{interaction_id}/#{interaction_token}/callback"

  def interaction_callback_original(application_id, interaction_token),
    do: "/webhooks/#{application_id}/#{interaction_token}/messages/@original"

  def interaction_followup_message(application_id, interaction_token, message_id),
    do: "/webhooks/#{application_id}/#{interaction_token}/#{message_id}"

  def channel_permissions(chanID), do: "/channels/#{chanID}/permissions"
  def channels, do: "/channels"
  def channel_call_ring(channel_id), do: "/channels/#{channel_id}/call/ring"
  def group_recipient(group_id, user_id), do: "/channels/#{group_id}/recipients/#{user_id}"
  def guild_me_nick(guild_id), do: "/guilds/#{guild_id}/members/@me/nick"

  def cdn_avatar(id, avatar, image_format), do: "/avatars/#{id}/#{avatar}.#{image_format}"
  def cdn_embed_avatar(image_name), do: "/embed/avatars/#{image_name}.png"
  def cdn_emoji(id, image_format), do: "/emojis/#{id}.#{image_format}"
  def cdn_icon(id, icon, image_format), do: "/icons/#{id}/#{icon}.#{image_format}"
  def cdn_splash(id, splash, image_format), do: "/splashes/#{id}/#{splash}.#{image_format}"
  def cdn_guild_banner(id, banner, image_format), do: "/banners/#{id}/#{banner}.#{image_format}"

  def cdn_discovery_splash(id, splash, image_format),
    do: "/discovery-splashes/#{id}/#{splash}.#{image_format}"

  def cdn_guild_avatar(guild_id, user_id, avatar_hash, image_format) do
    "/guilds/#{guild_id}/users/#{user_id}/avatars/#{avatar_hash}.#{image_format}"
  end

  def cdn_sticker(id, image_format), do: "/stickers/#{id}.#{image_format}"
  def cdn_sticker_pack(id), do: "/app-assets/710982414301790216/store/#{id}.png"

  def thread_with_message(channel_id, message_id),
    do: "/channels/#{channel_id}/messages/#{message_id}/threads"

  def thread_without_message(channel_id), do: "/channels/#{channel_id}/threads"

  def thread_member_me(thread_id), do: "/channels/#{thread_id}/thread-members/@me"

  def thread_member(thread_id, user_id), do: "/channels/#{thread_id}/thread-members/#{user_id}"

  def thread_members(thread_id), do: "/channels/#{thread_id}/thread-members"

  def guild_active_threads(guild_id), do: "/guilds/#{guild_id}/threads/active"

  def public_archived_threads(channel_id), do: "/channels/#{channel_id}/threads/archived/public"

  def private_archived_threads(channel_id), do: "/channels/#{channel_id}/threads/archived/private"

  def private_joined_archived_threads(channel_id),
    do: "/channels/#{channel_id}/users/@me/threads/archived/private"

  def guild_auto_moderation_rule(guild_id),
    do: "/guilds/#{guild_id}/auto-moderation/rules"

  def guild_auto_moderation_rule(guild_id, rule_id),
    do: "/guilds/#{guild_id}/auto-moderation/rules/#{rule_id}"

  def poll_answer_voters(channel_id, message_id, answer_id),
    do: "/channels/#{channel_id}/polls/#{message_id}/answers/#{answer_id}"

  def poll_expire(channel_id, message_id),
    do: "/channels/#{channel_id}/polls/#{message_id}/expire"

  def discord_epoch, do: 1_420_070_400_000

  def opcodes do
    %{
      "DISPATCH" => 0,
      "HEARTBEAT" => 1,
      "IDENTIFY" => 2,
      "STATUS_UPDATE" => 3,
      "VOICE_STATUS_UPDATE" => 4,
      "VOICE_SERVER_PING" => 5,
      "RESUME" => 6,
      "RECONNECT" => 7,
      "REQUEST_GUILD_MEMBERS" => 8,
      "INVALID_SESSION" => 9,
      "HELLO" => 10,
      "HEARTBEAT_ACK" => 11,
      "SYNC_GUILD" => 12,
      "SYNC_CALL" => 13
    }
  end

  def opcode_from_name(event) do
    opcodes()[event]
  end

  @spec atom_from_opcode(pos_integer()) :: atom()
  def atom_from_opcode(opcode) do
    {k, _} = Enum.find(opcodes(), fn {_, v} -> v == opcode end)
    k |> String.downcase() |> String.to_atom()
  end

  # Voice Gateway has a separate set of opcodes
  def voice_opcodes do
    %{
      "IDENTIFY" => 0,
      "SELECT_PROTOCOL" => 1,
      "READY" => 2,
      "HEARTBEAT" => 3,
      "SESSION_DESCRIPTION" => 4,
      "SPEAKING" => 5,
      "HEARTBEAT_ACK" => 6,
      "RESUME" => 7,
      "HELLO" => 8,
      "RESUMED" => 9,
      "UNDOCUMENTED_10" => 10,
      "UNDOCUMENTED_11" => 11,
      "CLIENT_CONNECT" => 12,
      "CLIENT_DISCONNECT" => 13,
      "CODEC_INFO" => 14,
      "UNDOCUMENTED_18" => 18
    }
  end

  def voice_opcode_from_name(event) do
    voice_opcodes()[event]
  end

  def atom_from_voice_opcode(opcode) do
    {k, _} = Enum.find(voice_opcodes(), {"UNKNOWN", -1}, fn {_, v} -> v == opcode end)
    k |> String.downcase() |> String.to_atom()
  end

  @doc """
  Return transport options for `:gun` to verify SSL certificates.

  By default, SSL connections will try to verify the peer, but not know
  where to look for a CA bundle. This will result in the following lines
  being printed to the log

  ```
  17:03:48.657 [warn]  Description: 'Authenticity is not established by certificate path validation'
     Reason: 'Option {verify, verify_peer} and cacertfile/cacerts is missing'
  ```

  This function returns a list suitable for the `:tls_opts` field of the
  `:gun.open` call. You can find more information about the type in [the
  gun documentation](https://ninenines.eu/docs/en/gun/2.0/manual/gun/).

  ## See also

  [Erlang Ecosystem Foundation: Secure coding and deployment
  hardening](https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/ssl)
  """
  @doc since: "0.5.0"
  @spec gun_tls_opts :: [:ssl.tls_client_option()]
  def gun_tls_opts,
    do: [
      verify: :verify_peer,
      cacerts: :certifi.cacerts(),
      depth: 3,
      customize_hostname_check: [match_fun: :public_key.pkix_verify_hostname_match_fun(:https)]
    ]

  defmodule ApplicationCommandType do
    @moduledoc """
    Defines available types used for selecting application command types
    For more info please refer to https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-types
    """

    def chat_input, do: 1
    def user, do: 2
    def message, do: 3
    def primary_entry_point, do: 4
  end

  defmodule ApplicationCommandOptionType do
    @moduledoc """
    Defines available types used for defining application command option types for passed options
    For more info please refer to https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-option-type
    """

    def sub_command, do: 1
    def sub_command_group, do: 2
    def string, do: 3
    def integer, do: 4
    def boolean, do: 5
    def user, do: 6
    def channel, do: 7
    def role, do: 8
    def mentionable, do: 9
    def number, do: 10
    def attachment, do: 11
  end

  defmodule ApplicationCommandPermissionType do
    @moduledoc """
    Defines available types for application command permissions
    For more info please refer to https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-application-command-permission-type
    """

    def role, do: 1
    def user, do: 2
    def channel, do: 3
  end

  defmodule ComponentType do
    @moduledoc """
    Defines available types for message components
    For more info please refer to https://discord.com/developers/docs/interactions/message-components#component-object-component-types
    """

    @spec action_row :: 1
    @doc "Container for other components"
    def action_row, do: 1

    @doc "Button object"
    def button, do: 2

    @doc "Select menu for picking from defined text options"
    def string_select, do: 3

    @doc "Text input object"
    def text_input, do: 4

    @doc "Select menu for users"
    def user_select, do: 5

    @doc "Select menu for roles"
    def role_select, do: 6

    @doc "Select menu for mentionables (users and roles)"
    def mentionable_select, do: 7

    @doc "Select menu for channels"
    def channel_select, do: 8
  end

  defmodule ButtonStyle do
    @moduledoc """
    Defines available styles for button message components
    For more info please refer to https://discord.com/developers/docs/interactions/message-components#button-object-button-styles
    """

    def primary, do: 1
    def secondary, do: 2
    def success, do: 3
    def danger, do: 4
    def link, do: 5
  end

  defmodule TextInputStyle do
    @moduledoc """
    Defines available styles for modal text inputs
    For more info please refer to https://discord.com/developers/docs/interactions/message-components#text-inputs-text-input-styles
    """

    def short, do: 1
    def paragraph, do: 2
  end

  defmodule InteractionType do
    @moduledoc """
    Defines available types for interactions
    For more info please refer to https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-interaction-type
    """

    def ping, do: 1
    def application_command, do: 2
    def message_component, do: 3
    def application_command_autocomplete, do: 4
    def modal_submit, do: 5
  end

  defmodule InteractionCallbackType do
    @moduledoc """
    Defines available types for interaction callbacks
    For more info please refer to https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-response-object-interaction-callback-type
    """

    def pong, do: 1
    def channel_message_with_source, do: 4
    def deferred_channel_message_with_source, do: 5
    def deferred_update_message, do: 6
    def update_message, do: 7
    def application_command_autocomplete_result, do: 8
    def modal, do: 9
  end

  defmodule ChannelType do
    @moduledoc """
    Defines available types for channels
    For more info please refer to https://discord.com/developers/docs/resources/channel#channel-object-channel-types
    """

    def guild_text, do: 0
    def dm, do: 1
    def guild_voice, do: 2
    def group_db, do: 3
    def guild_category, do: 4
    def guild_announcement, do: 5
    def announcement_thread, do: 10
    def public_thread, do: 11
    def private_thread, do: 12
    def guild_directory, do: 14
    def guild_forum, do: 15
  end

  defmodule WebhookType do
    @moduledoc """
    Defines available types for channels
    For more info please refer to https://discord.com/developers/docs/resources/webhook#webhook-object-webhook-types
    """

    def incoming, do: 1
    def channel_follower, do: 2
    def application, do: 3
  end
end
