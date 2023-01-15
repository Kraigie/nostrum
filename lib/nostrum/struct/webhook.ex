defmodule Nostrum.Struct.Webhook do
  @moduledoc """
  Struct representing a Discord webhook.
  """

  alias Nostrum.Struct.{Channel, Guild, User}

  @typedoc "Id of the webhook"
  @type id :: String.t()

  @typedoc "Guild the webhook is for"
  @type guild_id :: Guild.t()

  @typedoc "Channel the webhook is for"
  @type channel_id :: Channel.t()

  @typedoc "User who created the webhook"
  @type user :: User.t()

  @typedoc "Default name of the webhook"
  @type name :: integer

  @typedoc "Default avatar of the webhook"
  @type avatar :: integer

  @typedoc "Secure token of the webhook"
  @type token :: integer

  @type t :: %__MODULE__{
          id: id,
          guild_id: guild_id,
          channel_id: channel_id,
          user: user,
          name: name,
          avatar: avatar,
          token: token
        }

  @derive [Jason.Encoder]
  defstruct [
    :id,
    :guild_id,
    :channel_id,
    :user,
    :name,
    :avatar,
    :token
  ]

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.update(:user, %{}, &User.to_struct(&1))

    struct(__MODULE__, new)
  end
end
