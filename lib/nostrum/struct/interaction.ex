defmodule Nostrum.Struct.Interaction do
  @moduledoc "Slash command invocation struct."
  # https://discord.com/developers/docs/interactions/slash-commands#interaction

  alias Nostrum.Snowflake
  alias Nostrum.Struct.ApplicationCommandInteractionData
  alias Nostrum.Struct.Guild.Member
  alias Nostrum.Struct.{Channel, Guild}
  alias Nostrum.Util

  defstruct [
    :id,
    :type,
    :data,
    :guild_id,
    :channel_id,
    :member,
    :token,
    :version
  ]

  @typedoc "Interaction identifier"
  @type id :: Snowflake.t()

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
  @type guild_id :: Guild.id()

  @typedoc "ID of the channel where the command was invoked"
  @type channel_id :: Channel.id()

  @typedoc "Member information about the invoker"
  @type member :: Member.t()

  @typedoc "Continuation token for responses"
  @type token :: String.t()

  @typedoc "Version identifier, always `1`"
  @type version :: pos_integer()

  @typedoc """
  A command invocation for slash commands.

  Official reference:
  https://discord.com/developers/docs/interactions/slash-commands#interaction
  """
  @type t :: %__MODULE__{
          id: id,
          type: type,
          data: data,
          guild_id: guild_id,
          channel_id: channel_id,
          member: member,
          token: token,
          version: version
        }

  @doc false
  @spec to_struct(Map.t()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:member, nil, &Util.cast(&1, {:struct, Member}))

    struct(__MODULE__, new)
  end
end
