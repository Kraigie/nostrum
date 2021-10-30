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
  - `3` for *MessageComponent*
  - `4` for *ApplicationCommandAutocomplete*
  """
  @type type :: 1..4

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

  @typedoc "User object for the invoking user, will be a copy of `member.user` if invoked in a guild"
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
      user: Util.cast(map[:user] || map[:member][:user], {:struct, User}),
      token: map[:token],
      version: map[:version],
      message: Util.cast(map[:message], {:struct, Message})
    }
  end
end
