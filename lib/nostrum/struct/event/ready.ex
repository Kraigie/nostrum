defmodule Nostrum.Struct.Event.Ready do
  @moduledoc "Sent after initial handshake with the gateway"
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Event.PartialApplication
  alias Nostrum.Struct.Guild.UnavailableGuild
  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [:v, :user, :guilds, :session_id, :shard, :application]

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
  A pair of two integers ``{shard_id, num_shards}``.

  For more information, see
  https://discord.com/developers/docs/topics/gateway#sharding.
  """
  @type shard :: {Integer.t(), non_neg_integer()} | nil

  @typedoc "Partial application object with `id` and `flags`"
  @type application :: PartialApplication.t()

  @typedoc "Event sent after initial handshake with the gateway"
  @type t :: %__MODULE__{
          v: v,
          user: user,
          guilds: guilds,
          session_id: session_id,
          shard: shard,
          application: application
        }

  @doc false
  def to_struct(map) do
    %__MODULE__{
      v: map["v"],
      user: Util.cast(map["user"], {:struct, User}),
      guilds: Util.cast(map["guilds"], {:list, {:struct, UnavailableGuild}}),
      shard: :erlang.list_to_tuple(map["shard"]),
      application: Util.cast(map["application"], {:struct, PartialApplication})
    }
  end
end
