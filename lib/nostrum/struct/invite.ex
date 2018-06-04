defmodule Nostrum.Struct.Invite do
  @moduledoc """
  Struct representing a Discord invite.
  """

  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Invite.Metadata
  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [
    :code,
    :guild,
    :channel,
    :inviter,
    :metadata
  ]

  @typedoc """
  The invite code (unique ID).
  """
  @type code :: String.t()

  @typedoc """
  The guild this invite is for.
  """
  @type guild :: Guild.t()

  @typedoc """
  The channel this invite is for.
  """
  @type channel :: Channel.t()

  @typedoc """
  The user who created this invite.
  """
  @type inviter :: User.t()

  @typedoc """
  The extra metadata this invite contains.

  This field is `nil` unless this invite is returned by
  `Nostrum.Api.get_channel_invites/1`.
  """
  @type metadata :: Metadata.t()

  @type simple_invite :: %__MODULE__{
          code: code,
          guild: guild,
          channel: channel,
          inviter: inviter,
          metadata: nil
        }

  @type detailed_invite :: %__MODULE__{
          code: code,
          guild: guild,
          channel: channel,
          inviter: inviter,
          metadata: metadata
        }

  @type t :: simple_invite | detailed_invite

  @doc false
  def to_struct(map) do
    atom_map = map |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    invite =
      struct(__MODULE__, atom_map)
      |> Map.update(:guild, nil, &Util.cast(&1, {:struct, Guild}))
      |> Map.update(:channel, nil, &Util.cast(&1, {:struct, Channel}))
      |> Map.update(:inviter, nil, &Util.cast(&1, {:struct, User}))
      |> cast_metadata(atom_map)

    invite
  end

  defp cast_metadata(invite, map) do
    metadata_keys = [:uses, :max_uses, :max_age, :temporary, :created_at, :revoked]

    if Enum.any?(map, fn {k, _} -> Enum.member?(metadata_keys, k) end) do
      %{invite | metadata: Util.cast(map, {:struct, Metadata})}
    else
      invite
    end
  end
end
