defmodule Nostrum.Struct.Event.Ready do
  @moduledoc "Sent after initial handshake with the gateway"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Event.PartialApplication
  alias Nostrum.Struct.Guild.UnavailableGuild
  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [:v, :user, :guilds, :session_id, :shard, :application, :resume_gateway_url]

  @typedoc """
  Gateway version.
  See https://discord.com/developers/docs/topics/gateway#gateways-gateway-versions
  """
  @type v :: non_neg_integer()

  @typedoc "Information about the bot user"
  @type user :: User.t()

  @typedoc "The guilds that the bot user is in"
  @type guilds :: [UnavailableGuild.t()]

  @typedoc """
  Used for resuming connections.

  If you are wondering whether you need to use this, you probably don't.
  Nostrum handles reconnections for you.
  """
  @type session_id :: String.t()

  @typedoc """
  Similar to `t:session_id/0`, this is the URL that Discord has requested
  reconnection attempts to be made against.

  Nostrum stores and handles this for you, this value is provided for
  debugging purposes.
  """
  @type resume_gateway_url :: String.t()

  @typedoc """
  A pair of two integers ``{shard_id, num_shards}``.

  For more information, see
  https://discord.com/developers/docs/topics/gateway#sharding.
  """
  @type shard :: {integer(), non_neg_integer()} | nil

  @typedoc "Partial application object with `id` and `flags`"
  @type application :: PartialApplication.t()

  @typedoc "Event sent after initial handshake with the gateway"
  @type t :: %__MODULE__{
          v: v,
          user: user,
          guilds: guilds,
          session_id: session_id,
          shard: shard,
          application: application,
          resume_gateway_url: resume_gateway_url
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, User}))
      |> Map.update(:guilds, nil, &Util.cast(&1, {:list, {:struct, UnavailableGuild}}))
      |> Map.update(:application, nil, &Util.cast(&1, {:struct, PartialApplication}))
      |> Map.update(:shard, nil, &:erlang.list_to_tuple/1)

    struct(__MODULE__, new)
  end
end
