# Gateway Intents

The Discord API allows you to fine-tune the payloads that are received by your bot through gateway intents.

When connecting to Discord you can instruct Discord on which events you want to receive (i.e. messages, user presences, etc.).

Nostrum allows you to set these intents within your bots' options.

To pass intents, configure them in your supervisor tree as part of
`t:Nostrum.Bot.bot_options/0`, via the `:intents` field.

Possible intents (and the gateway events they correspond to) are:

```
guilds:
  - GUILD_CREATE
  - GUILD_UPDATE
  - GUILD_DELETE
  - GUILD_ROLE_CREATE
  - GUILD_ROLE_UPDATE
  - GUILD_ROLE_DELETE
  - CHANNEL_CREATE
  - CHANNEL_UPDATE
  - CHANNEL_DELETE
  - CHANNEL_PINS_UPDATE
  - THREAD_CREATE
  - THREAD_UPDATE
  - THREAD_DELETE
  - THREAD_LIST_SYNC
  - THREAD_MEMBER_UPDATE
  - THREAD_MEMBERS_UPDATE
  - STAGE_INSTANCE_CREATE
  - STAGE_INSTANCE_UPDATE
  - STAGE_INSTANCE_DELETE

guild_members*:
  - GUILD_MEMBER_ADD
  - GUILD_MEMBER_UPDATE
  - GUILD_MEMBER_REMOVE
  - THREAD_MEMBERS_UPDATE

guild_moderation:
  - GUILD_AUDIT_LOG_ENTRY_CREATE
  - GUILD_BAN_ADD
  - GUILD_BAN_REMOVE

guild_expressions:
  - GUILD_EMOJIS_UPDATE
  - GUILD_STICKERS_UPDATE
  - GUILD_SOUNDBOARD_SOUND_CREATE
  - GUILD_SOUNDBOARD_SOUND_UPDATE
  - GUILD_SOUNDBOARD_SOUND_DELETE
  - GUILD_SOUNDBOARD_SOUNDS_UPDATE

guild_integrations:
  - GUILD_INTEGRATIONS_UPDATE
  - INTEGRATION_CREATE
  - INTEGRATION_UPDATE
  - INTEGRATION_DELETE

guild_webhooks:
  - WEBHOOKS_UPDATE

guild_invites:
  - INVITE_CREATE
  - INVITE_DELETE

guild_voice_states:
  - VOICE_CHANNEL_EFFECT_SEND
  - VOICE_STATE_UPDATE

guild_presences*:
  - PRESENCE_UPDATE

guild_messages:
  - MESSAGE_CREATE
  - MESSAGE_UPDATE
  - MESSAGE_DELETE
  - MESSAGE_DELETE_BULK

guild_message_reactions:
  - MESSAGE_REACTION_ADD
  - MESSAGE_REACTION_REMOVE
  - MESSAGE_REACTION_REMOVE_ALL
  - MESSAGE_REACTION_REMOVE_EMOJI

guild_message_typing:
  - TYPING_START

direct_messages:
  - MESSAGE_CREATE
  - MESSAGE_UPDATE
  - MESSAGE_DELETE
  - CHANNEL_PINS_UPDATE

direct_message_reactions:
  - MESSAGE_REACTION_ADD
  - MESSAGE_REACTION_REMOVE
  - MESSAGE_REACTION_REMOVE_ALL
  - MESSAGE_REACTION_REMOVE_EMOJI

direct_message_typing:
  - TYPING_START

message_content*:
  - MESSAGE_CONTENT ***

guild_scheduled_events:
  - GUILD_SCHEDULED_EVENT_CREATE
  - GUILD_SCHEDULED_EVENT_UPDATE
  - GUILD_SCHEDULED_EVENT_DELETE
  - GUILD_SCHEDULED_EVENT_USER_ADD
  - GUILD_SCHEDULED_EVENT_USER_REMOVE

auto_moderation_configuration:
  - AUTO_MODERATION_RULE_CREATE
  - AUTO_MODERATION_RULE_DELETE
  - AUTO_MODERATION_RULE_UPDATE

auto_moderation_execution:
  - AUTO_MODERATION_RULE_EXECUTION

guild_message_polls:
  - MESSAGE_POLL_VOTE_ADD
  - MESSAGE_POLL_VOTE_REMOVE

direct_message_polls:
  - MESSAGE_POLL_VOTE_ADD
  - MESSAGE_POLL_VOTE_REMOVE
```

Besides an explicit list of atoms, acceptable configuration values are `:all` and `:nonprivileged`.

Note that intents marked with `*` are **privileged** intents. You must enable these intents by visiting the Discord Developer portal.
Also note that `MESSAGE_CONTENT` is not an event type, but including the `:message_content` intent will change the data received in 
message-related events.

Once your bot passes 100 servers Discord will force you to **verify your bot**. If you require privileged intents you may need to write additional information on your usage of them.

The `:nonprivileged` option is equivalent to setting the bot's `:intents` to the entirety of the above 
list except for the **privileged** intents.  If you require all intents, including
privileged ones, set `:intents` to `:all`.

More information on gateway intents can be found in the [Discord Developer documentation](https://discord.com/developers/docs/topics/gateway#gateway-intents).
