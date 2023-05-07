defmodule Nostrum.Struct.ApplicationCommand do
  @moduledoc """
  Typespecs for creating Application Commands

  Official reference:
  https://discord.com/developers/docs/interactions/application-commands
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Snowflake

  @typedoc """
  The name of the command, subcommand, or command_option.
  It must be between 1 and 32 characters in length and match the following regex: `^[\w-]{1,32}$`.
  Only `USER` and `MESSAGE` commands may include uppercase letters and spaces.
  """
  @type command_name :: String.t()

  @typedoc """
  The description of the command, subcommand, or command_option.
  For `CHAT_INPUT` commands, it must be between 1 and 100 characters in length.
  For `USER` and `MESSAGE` commands it must be an empty string.
  """
  @type command_description :: String.t()

  @typedoc """
  The type of application command you wish to create
  - `1` for `CHAT_INPUT`, regular slash commands (default)
  - `2` for `USER`, right-click menu commands on a specific user
  - `3` for `MESSAGE`, right-click menu commands on a specific message

  You may use one of the `Nostrum.Constants.ApplicationCommandType` methods.
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
  Indicates what type of argument the command expects.

  - `1` for `SUB_COMMAND`
  - `2` for `SUB_COMMAND_GROUP`
  - `3` for `​STRING`
  - `4` for `​INTEGER` *Note*: due to API limitations they can only be between -2^53 and 2^53
  - `5` for `BOOLEAN`
  - `6` for `USER`
  - `7` for `CHANNEL`
  - `8` for `ROLE`
  - `9` for `MENTIONABLE` *Note*: Includes users and roles
  - `10` for `NUMBER` *Note*: This has the same limitations as `​INTEGER`

  You may use one of the `Nostrum.Constants.ApplicationCommandOptionType` methods.
  """
  @type command_option_type :: 1..10

  @typedoc """
  This defines a command's parameters. Only valid for `CHAT_INPUT` commands.

  ## Notes
   - required parameters on a command must precede optional ones
   - for subcommands and subcommand groups, `:options` are its parameters
   - `:options` and `:choices` are mutually exclusive
   - `:autocomplete` must not be set to true if `:choices` is present
   - if `:type` is 7 then `:channel_types` can be a list of allowed [channel types](https://discord.com/developers/docs/resources/channel#channel-object-channel-types)

  """
  @type command_option :: %{
          required(:name) => command_name(),
          required(:description) => command_description(),
          required(:type) => command_option_type(),
          optional(:required) => boolean(),
          optional(:choices) => [command_choice()],
          optional(:options) => [command_option()],
          optional(:channel_types) => [pos_integer()],
          optional(:autocomplete) => boolean()
        }

  @typedoc """
  This defines the map for creating an application command.

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

  @typedoc """
  When editing an existing application command, only the following fields may be updated,
  missing fields will remain unchanged.
  """
  @type application_command_edit_map :: %{
          optional(:name) => command_name(),
          optional(:description) => command_description(),
          optional(:options) => [command_option()],
          optional(:default_permission) => boolean()
        }

  @typedoc """
  For editing the permissions for an application command
   - `:id` is the id of the role or user
   - `:type` is the type of the id, either `role` or `user`
   - `:allow` is whether the role or user should be allowed to use the command
  """
  @type application_command_permissions :: %{
          id: Snowflake.t(),
          type: application_command_permission_type(),
          permission: boolean()
        }

  @typedoc """
  - `1` for `ROLE`
  - `2` for `USER`
  - `3` for `CHANNEL`

  You can use one of the `Nostrum.Constants.ApplicationCommandPermissionType` methods.
  """
  @type application_command_permission_type :: 1..3
end
