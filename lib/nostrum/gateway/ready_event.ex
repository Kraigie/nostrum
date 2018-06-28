defmodule Nostrum.Gateway.ReadyEvent do
  alias Nostrum.Util
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.User
  alias Nostrum.Struct.Guild

  defstruct [
    :v,
    :user,
    :private_channels,
    :guilds,
    :session_id,
    :_trace
  ]

  defmodule Field do
    @moduledoc false

    @spec new({term, term}) :: Keyword.t()
    def new(pair)
    def new({k, v}) when not is_binary(k), do: new({to_string(k), v})
    def new({"v", v}), do: [v: v]
    def new({"user", user}), do: [user: Util.cast(user, {:struct, User})]
    def new({"guilds", guilds}), do: [guilds: Util.cast(guilds, {:list, {:struct, Guild}})]
    def new({"session_id", session_id}), do: [session_id: session_id]
    def new({"_trace", trace}), do: [_trace: trace]

    def new({"private_channels", value}),
      do: [private_channels: Util.cast(value, {:list, {:struct, Channel}})]

    def new({"d", data}), do: data |> Enum.map(&new(&1)) |> Enum.concat()

    def new(_), do: []
  end

  @type t :: %__MODULE__{
          v: String.t(),
          user: User.t(),
          private_channels: [Channel.dm_channel()],
          guilds: [Guild.t()],
          session_id: String.t(),
          _trace: [String.t()]
        }

  @spec new(Enum.t()) :: t
  def new(enum) do
    fields =
      enum
      |> Enum.map(&Field.new(&1))
      |> Enum.concat()

    struct(__MODULE__, fields)
  end
end
