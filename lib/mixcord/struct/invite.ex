defmodule Mixcord.Struct.Invite do
  @moduledoc """
  Struct representing a Discord invite.
  """

  alias Mixcord.Struct.Invite.{Channel, Guild}
  alias Mixcord.Struct.User

  @typedoc "Invite code"
  @type code :: String.t

  @typedoc "Guild the invite is for"
  @type guild :: Guild.t

  @typedoc "Channel the invite is for"
  @type channel :: Channel.t

  @typedoc "User who created the invite"
  @type inviter :: User.t

  @typedoc "Number of times this invite has been used"
  @type uses :: integer

  @typedoc "Number of times this invite can be used"
  @type max_uses :: integer

  @typedoc "Duration in seconds after which the invite expires"
  @type max_age :: integer

  @typedoc "Whether the invite gives temporary membership"
  @type temporary :: boolean

  @typedoc "When the invite was created"
  @type created_at :: String.t

  @typedoc "Whether the invite is revoked"
  @type revoked :: boolean

  @type t :: %__MODULE__{
    code: code,
    guild: guild,
    channel: channel,
    inviter: inviter,
    uses: uses,
    max_uses: max_uses,
    max_age: max_age,
    temporary: temporary,
    created_at: created_at,
    revoked: revoked
  }

  @derive [Poison.Encoder]
  defstruct [
    :code,
    :guild,
    :channel,
    :inviter,
    :uses,
    :max_uses,
    :max_age,
    :temporary,
    :created_at,
    :revoked
  ]

  @doc false
  def to_struct(map) do
    new = map
    |> Map.update(:guild, %{}, &Guild.to_struct(&1))
    |> Map.update(:channel, %{}, &Channel.to_struct(&1))
    |> Map.update(:inviter, %{}, &User.to_struct(&1))
    struct(__MODULE__, new)
  end
end
