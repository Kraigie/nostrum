defmodule Nostrum.Shard.Intents do
  @moduledoc false

  import Bitwise

  @privileged_intents [
    :guild_members,
    :guild_presences
  ]

  @spec intent_values :: [{atom, integer()}, ...]
  def intent_values do
    [
      guilds: 1 <<< 0,
      guild_members: 1 <<< 1,
      guild_bans: 1 <<< 2,
      guild_emojis: 1 <<< 3,
      guild_integrations: 1 <<< 4,
      guild_webhooks: 1 <<< 5,
      guild_invites: 1 <<< 6,
      guild_voice_states: 1 <<< 7,
      guild_presences: 1 <<< 8,
      guild_messages: 1 <<< 9,
      guild_message_reactions: 1 <<< 10,
      guild_message_typing: 1 <<< 11,
      direct_messages: 1 <<< 12,
      direct_message_reactions: 1 <<< 13,
      direct_message_typing: 1 <<< 14
    ]
  end

  @spec get_enabled_intents :: integer()
  def get_enabled_intents do
    # If no intents are passed in config, default to non-privileged being enabled.
    enabled_intents = Application.get_env(:nostrum, :gateway_intents, :nonprivileged)

    case enabled_intents do
      :all ->
        get_intent_value(Keyword.keys(intent_values()))

      :nonprivileged ->
        get_intent_value(Keyword.keys(intent_values()) -- @privileged_intents)

      intents ->
        get_intent_value(intents)
    end
  end

  @spec get_intent_value([atom()]) :: integer
  def get_intent_value(enabled_intents) do
    Enum.reduce(enabled_intents, 0, fn intent, intents ->
      case intent_values()[intent] do
        nil -> raise "Invalid intent specified: #{intent}"
        value -> intents ||| value
      end
    end)
  end

  @spec has_intent?(atom()) :: boolean
  def has_intent?(requested_intent) do
    enabled_integer = get_enabled_intents()
    intent_integer = intent_values()[requested_intent]

    (enabled_integer &&& intent_integer) == intent_integer
  end
end
