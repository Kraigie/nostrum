defmodule Nostrum.Struct.Event.GuildBanRemove do
  @moduledoc "Sent when a user is unbanned from a guild"

  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [:guild_id, :user]

  @typedoc "ID of the guild"
  @type guild_id :: Guild.id()

  @typedoc "Unbanned user"
  @type user :: User.t()

  @typedoc "Event sent when a user is unbanned from a guild"
  @type t :: %__MODULE__{
          guild_id: guild_id,
          user: user
        }

  @doc false
  def to_struct(map) do
    %__MODULE__{
      guild_id: map["guild_id"],
      user: Util.cast(map["user"], {:struct, User})
    }
  end
end
