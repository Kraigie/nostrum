defmodule Nostrum.Struct.Interaction do
  @moduledoc "Slash command invocation struct."
  # https://discord.com/developers/docs/interactions/slash-commands#interaction

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

  @typedoc "ID of the application that this interaction is for"
  @typedoc since: "0.5.0"
  @type application_id :: Snowflake.t()

  @typedoc """
  Interaction kind.

  - `1` for *Ping*
  - `2` for *ApplicationCommand*
  """
  @type type :: pos_integer()

  @typedoc """
  Invocation data.

  Only present for *ApplicationCommand* interactions, that is, `type=2`.
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

  @typedoc "Continuation token for responses"
  @type token :: String.t()

  @typedoc "Version identifier, always `1`"
  @type version :: pos_integer()

  @typedoc "For components, the message they were attached to"
  @typedoc since: "0.5.0"
  @type message :: Message.t() | nil

  @typedoc """
  A command invocation for slash commands.

  Official reference:
  https://discord.com/developers/docs/interactions/slash-commands#interaction
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
      application_id: map.application_id,
      type: map.type,
      data: Util.cast(map[:data], {:struct, ApplicationCommandInteractionData}),
      guild_id: map[:guild_id],
      channel_id: map[:channel_id],
      member: Util.cast(map[:member], {:struct, Member}),
      user: Util.cast(map[:user], {:struct, User}),
      token: map.token,
      version: map.version,
      message: Util.cast(map[:message], {:struct, Message})
    }
  end
end
