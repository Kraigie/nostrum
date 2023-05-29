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
    :channel,
    :member,
    :user,
    :token,
    :version,
    :message,
    :locale,
    :guild_locale
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
  - `5` for *ModalSubmit*
  """
  @type type :: 1..5

  @typedoc """
  Invocation data.

  Only present for *ApplicationCommand* and *MessageComponent* interactions, that is, `type=2` or `type=3`.
  """
  @type data :: ApplicationCommandInteractionData.t() | nil

  @typedoc "ID of the guild where the command was invoked"
  @type guild_id :: Guild.id() | nil

  @typedoc "ID of the channel where the command was invoked"
  @type channel_id :: Channel.id()

  @typedoc """
  PartialChannel object for the channel where the command was invoked

  At the time of writing, only the fields `:type` and `:id` are guaranteed to be present.
  """
  @typedoc since: "0.8.0"
  @type channel :: Channel.t() | nil

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
  The selected langauge of the invoking user.

  Available on all interaction types except for *PING*
  """
  @typedoc since: "0.6.0"
  @type locale :: String.t() | nil

  @typedoc """
  The guild's preferred locale, if invoked in a guild.
  """
  @typedoc since: "0.6.0"
  @type guild_locale :: String.t() | nil

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
          channel: channel,
          member: member,
          user: user,
          token: token,
          version: version,
          message: message,
          locale: locale,
          guild_locale: guild_locale
        }

  @doc false
  @spec to_struct(map()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:application_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel, nil, &Util.cast(&1, {:struct, Channel}))
      |> Map.update(:data, nil, &Util.cast(&1, {:struct, ApplicationCommandInteractionData}))
      |> Map.update(:member, nil, &Util.cast(&1, {:struct, Guild.Member}))
      |> Map.put_new(:user, map[:member][:user])
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, User}))
      |> Map.update(:message, nil, &Util.cast(&1, {:struct, Message}))

    struct(__MODULE__, new)
  end
end
