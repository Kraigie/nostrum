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
commands. Global commands used to take up to an hour to update across all
guilds, but that is not the case anymore.

We will create a command that will allow the user to assign or remove a role of
choice. Because each command resides in its own module, we'll have to write one:

```elixir
defmodule MyBot.Command.Role do
  alias Nostrum.{Command, Command.Spec.Option}
  use Command, %Command.Spec{
    name: "role",
    desc: "assign or remove a role",
    options: [
      %Option{
        name: "name",
        desc: "role to assign or remove",
        type: :role
      },
      %Option{
        name: "action",
        desc: "whether to assign or remove the role",
        type: :string,
        choices: [
          %{name: "assign", value: "assign"},
          %{name: "remove", value: "remove"}
        ]
      }
    ]
  }
end
```

To register this command globally, we need to specify it in `config/config.exs`:
```elixir
import Config
# ...
config :nostrum,
  token: "YouЯ.T0.k3n",
  managed_commands: [
    MyBot.Command.Role
  ]
```

## Receiving interactions

When Nostrum receives an interaction, it invokes the function ``handle/2`` of
your command module with the interaction and options it extracted from that
interaction. Let's write a function with two clauses to handle our command:

```elixir
defmodule MyBot.Command.Role do
  alias Nostrum.{Command, Command.Spec.Option, Api} # note the new alias!
  use Command, %Command.Spec{
    # ... spec from before
  }

  def handle(interaction, %{"action" => "assign", "name" => role_id}) do
    Api.add_guild_member_role(interaction.guild_id, interaction.member.user.id, role_id)
    :ignore
  end

  def handle(interaction, %{"action" => "remove", "name" => role_id}) do
    Api.remove_guild_member_role(interaction.guild_id, interaction.member.user.id, role_id)
    :ignore
  end
end
```

Try starting your bot running the command. Does it work?

Behind the hood, an ``:INTERACTION_CREATE`` event was received by a built-in
Nostrum consumer with an interaction that looked something like the following:

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

Nostrum figured out which module to call to handle that command and converted the
option list to a map.

## Responding to interactions

Did you notice the ``:ignore`` at the end of our function clauses? It's telling
Nostrum to not send anything back to the user. Although our command performs
its job, the lack of a response forces Discord to display "YourBot is thinking"
to the user for 15 minutes until eventually giving up and switching that to
"The application did not respond".

To respond with something more meaningful, simply return a map like the
following:

```elixir
def handle(interaction, %{"action" => "assign", "name" => role_id}) do
  # ...
  %{content: ":white_check_mark: **role assigned**"}
end

def handle(interaction, %{"action" => "remove", "name" => role_id}) do
  # ...
  %{content: ":white_check_mark: **role removed**"}
end
```

Aside from `content`, you can also specify `embeds` and `components`.

We have now built a simple command using slash commands, with argument
conversion delegated to Discords side of things. Further actions on the
command, such as checking permissions, author roles, and more - are left as an
exercise to the reader.
