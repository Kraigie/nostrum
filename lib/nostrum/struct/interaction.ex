defmodule Nostrum.Struct.Interaction do
  @moduledoc "Application command and Component invocation struct."
  # https://discord.com/developers/docs/interactions/application-commands#interaction

  alias Nostrum.Snowflake
  alias Nostrum.Struct.ApplicationCommandInteractionData
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.User
  alias Nostrum.Struct.{Channel, Guild, Message}
  alias Nostrum.Util

  defstruct [
    :id,
    :application_id,
    :type,
    :data,
    :guild_id,
    :channel_id,
    :member,
    :user,
    :token,
    :version,
    :message
  ]

  @typedoc "Interaction identifier"
  @type id :: Snowflake.t()

  @typedoc """
  ID of the application that this interaction is for

  Will be `nil` if the interaction was part of a message struct.
  """
  @typedoc since: "0.5.0"
  @type application_id :: Snowflake.t() | nil

  @typedoc """
  Interaction kind.

  - `1` for *Ping*
  - `2` for *ApplicationCommand*
  - `3` for *MessageComponent*`
  """
  @type type :: pos_integer()

  @typedoc """
  Invocation data.

  Only present for *ApplicationCommand* and *MessageComponent* interactions, that is, `type=2` or `type=3`.
  """
  @type data :: ApplicationCommandInteractionData.t() | nil

  @typedoc "ID of the guild where the command was invoked"
  @type guild_id :: Guild.id() | nil

  @typedoc "ID of the channel where the command was invoked"
  @type channel_id :: Channel.id()

  @typedoc "Member information about the invoker, if invoked on a guild"
  @type member :: Member.t() | nil

  @typedoc "User object for the invoking user, if invoked via a DM"
  @typedoc since: "0.5.0"
  @type user :: User.t() | nil

  @typedoc """
  Continuation token for responses

  Will be `nil` if this interaction is part of a message struct.
  """
  @type token :: String.t() | nil

  @typedoc """
  Version identifier, always `1`

  Will be `nil` if this interaction is part of a message struct.
  """
  @type version :: pos_integer() | nil

  @typedoc "For components, the message they were attached to"
  @typedoc since: "0.5.0"
  @type message :: Message.t() | nil

  @typedoc """
  A command invocation for Application Commands or Components.

  Official reference:
  https://discord.com/developers/docs/interactions/application-commands
  """
  @type t :: %__MODULE__{
          id: id,
          application_id: application_id,
          type: type,
          data: data,
          guild_id: guild_id,
          channel_id: channel_id,
          member: member,
          user: user,
          token: token,
          version: version,
          message: message
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    %__MODULE__{
      id: map.id,
      application_id: map[:application_id],
      type: map.type,
      data: Util.cast(map[:data], {:struct, ApplicationCommandInteractionData}),
      guild_id: map[:guild_id],
      channel_id: map[:channel_id],
      member: Util.cast(map[:member], {:struct, Member}),
      user: Util.cast(map[:user], {:struct, User}),
      token: map[:token],
      version: map[:version],
      message: Util.cast(map[:message], {:struct, Message})
    }
  end
end

defmodule Nostrum.Struct.ApplicationCommand do
  @moduledoc """
  typespecs for creating Application Commands

  Official reference:
  https://discord.com/developers/docs/interactions/application-commands
  """

  @typedoc """
  The name of the command, subcommand, or command_option, it must be between 1 and 32 characters in length and match the following regex `^[\w-]{1,32}$`
  Only `USER` and `MESSAGE` commands may include uppercase letters and spaces.
  """
  @type command_name :: String.t()

  @typedoc """
  The command, subcommand, or options's description, for `CHAT_INPUT` commands it must be between 1 and 100 characters in length.
  For `USER` and `MESSAGE` commands it must be an empty string.
  """
  @type command_description :: String.t()

  @typedoc """
  The type of application command you wish to create
  - `1` for `CHAT_INPUT`, regular slash commands (default)
  - `2` for `USER`, right-click menu commands on a specific user
  - `3` for `MESSAGE`, right-click menu commands on a specific message
  """
  @type command_type :: pos_integer()

  @typedoc """
  If you specify choices for a command those become the only valid options for the user to select from.
  """
  @type command_choice :: %{
          name: String.t(),
          value: String.t() | number()
        }

  @typedoc """
  - `1` for `SUB_COMMAND`
  - `2` for `SUB_COMMAND_GROUP`
  - `3` for `STRING`
  - `4` for `INTEGER` *Note*: due to API limitations they can only be between -2^53 and 2^53
  - `5` for `BOOLEAN`
  - `6` for `USER`
  - `7` for `CHANNEL`
  - `8` for `ROLE`
  - `9` for `MENTIONABLE` *Note*: Includes users and roles
  - `10` for `NUMBER` *Note*: This has the same limitations as `INTEGER`
  """
  @type command_option_type :: pos_integer()

  @typedoc """
  This defines a commands parameters, only valid for `CHAT_INPUT` commands.

  *Notes*:
   - required parameters on a command must precede optional ones
   - for subcommands and subcommand groups, `:options` are it's parameters
   - `:options` and `:choices` are mutually exclusive
   - if `:type` is 7 then `:channel_types` can be a list of allowed [channel types](https://discord.com/developers/docs/resources/channel#channel-object-channel-types)

  """
  @type command_option :: %{
          required(:name) => command_name(),
          required(:description) => command_description(),
          required(:type) => command_option_type(),
          optional(:required) => boolean(),
          optional(:choices) => [command_choice()],
          optional(:options) => [command_option()],
          optional(:channel_types) => [pos_integer()]
        }

  @typedoc """
  This defindes the map for creating an application command.

  `:default_permission` is for if the command is enabled for all users by default

  For more information see [the official documentation](https://discord.com/developers/docs/interactions/application-commands)
  """
  @type application_command_map :: %{
          required(:name) => command_name(),
          required(:description) => command_description(),
          optional(:type) => command_type(),
          optional(:default_permission) => boolean(),
          optional(:options) => [command_option()]
        }
end
