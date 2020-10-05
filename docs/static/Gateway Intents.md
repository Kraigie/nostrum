# Gateway Intents

The Discord API allows you to fine tune the payloads that are received by your bot through gateway intents.

When connecting to Discord you can instruct Discord on which events you want to receive (i.e. messages, user presences, etc.).

Nostrum allows you to set these intents within your configuration, **by default all intents are enabled, including privileged ones**.

To pass intents you should use the following configuration:
```elixir
config :nostrum,
  token: "bot_token",
  num_shards: :auto,
  gateway_intents: [
      :guilds,
      # other gateway intents
  ]
```

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

guild_members*:
  - GUILD_MEMBER_ADD
  - GUILD_MEMBER_UPDATE
  - GUILD_MEMBER_REMOVE

guild_bans:
  - GUILD_BAN_ADD
  - GUILD_BAN_REMOVE

guild_emojis:
  - GUILD_EMOJIS_UPDATE

guild_integrations:
  - GUILD_INTEGRATIONS_UPDATE

guild_webhooks:
  - WEBHOOKS_UPDATE

guild_invites:
  - INVITE_CREATE
  - INVITE_DELETE

guild_voice_states:
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
```

Note that intents marked with `*` are **privileged** intents. You must enable these intents by visiting the Discord Developer portal.

Once your bot passes 100 servers Discord will force you to **verify your bot**. If you require privileged intents you may need to write additional information on your usage of them.

Be advised that since Nostrum defaults to all intents being enabled you may need to disable these intents by adding everything else in the above list to the `gateway_intents` configuration option.

More information on gateway intents can be found in the [Discord Developer documentation](https://discord.com/developers/docs/topics/gateway#gateway-intents).
