# Application commands

Discord allows you to create commands for your bot that work within the slash
command menu or via the context menu.

Using nostrum, you can create, receive, and respond to application commands invoked
by Discord users.

## Permissions required

Quoting the [official Discord
documentation](https://discord.com/developers/docs/interactions/application-commands#authorizing-your-application):

> In order to make Commands work within a guild, the guild must authorize
> your application with the `applications.commands` scope. The `bot` scope is not
> enough.

## Getting started

Discord differentiates between **global** and **guild-specific** slash
commands. Global commands will be distributed across all guilds that your bot
is in within an hour. Guild-specific commands slash commands will be available
instantly, which is why we will use guild-specific commands for testing.

We will create a command that will allow the user to assign or remove a role of
choice. The `guild_id` parameter is the ID of the guild on which the command
will be created.

Our command definition looks as follows:

```elixir
command = %{
  name: "role",
  description: "assign or remove a role",
  options: [
    %{
      # ApplicationCommandType::ROLE
      type: 8,
      name: "name",
      description: "role to assign or remove",
      required: true
    },
    %{
      # ApplicationCommandType::STRING
      type: 3,
      name: "action",
      description: "whether to assign or remove the role",
      required: true,
      choices: [
        %{
          name: "assign",
          value: "assign"
        },
        %{
          name: "remove",
          value: "remove"
        }
      ]
    }
  ]
}
```

To register this command on the guild, we simply pass it to
`Nostrum.Api.ApplicationCommand.create_guild_command/2`:

```elixir
Nostrum.Api.ApplicationCommand.create_guild_command(guild_id, command)
```

You can register the command in the ``:READY`` gateway event handler.

## Receiving interactions

Set up a gateway event handler for ``:INTERACTION_CREATE``. On command
invocation the interaction payload will look something like the following:

```elixir
%Nostrum.Struct.Interaction{
  channel_id: 474025345243414539,
  data: %{
    id: 793152718839087135,
    name: "role",
    options: [
      %{name: "name", value: "458692275199803406"},
      %{name: "action", value: "assign"}
    ]
  },
  # ...
```

Note that Discord already converted the user-supplied role to a snowflake.
Convenient!

Let's match on the retrieved event and create two function heads for the
separate operation modes:

```elixir
alias Nostrum.Api
alias Nostrum.Api.Role
alias Nostrum.Struct.Interaction

defp manage_role(%Interaction{data: %{options: [%{value: role_id}, %{value: "assign"}]}} = interaction) do
  Role.add_member(interaction.guild_id, interaction.member.user_id, role_id)
end

defp manage_role(%Interaction{data: %{options: [%{value: role_id}, %{value: "remove"}]}} = interaction) do
  Role.remove_member(interaction.guild_id, interaction.member.user_id, role_id)
end

def handle_event({:INTERACTION_CREATE, %Interaction{data: %{name: "role"}} = interaction, _ws_state}) do
  manage_role(interaction)
end
```

Okay, we now have our handling code done. This is pretty much the same code
that you would use for regular commands.


## Responding to interactions

To respond to interactions, use ``Nostrum.Api.Interaction.create_response/2``:

```elixir
defp manage_role(%Interaction{data: %{options: [%{value: role_id}, %{value: "assign"}]}} = interaction) do
  Role.add_member(interaction.guild_id, interaction.member.user_id, role_id)
  response = %{
    type: 4,  # ChannelMessageWithSource
    data: %{
      content: "role assigned"
    }
  }
  Api.Interaction.create_response(interaction, response)
end
```

We have now built a simple command using slash commands, with argument
conversion delegated to Discords side of things. Further actions on the
command, such as checking permissions, author roles, and more - are left as an
exercise to the reader.
