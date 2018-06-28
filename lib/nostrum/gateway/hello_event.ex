defmodule Nostrum.Gateway.HelloEvent do
  defstruct [
    :heartbeat_interval,
    :_trace
  ]

  defmodule Field do
    @moduledoc false

    @spec new({term, term}) :: Keyword.t()
    def new(pair)
    def new({k, v}) when not is_binary(k), do: new({to_string(k), v})
    def new({"heartbeat_interval", value}), do: [heartbeat_interval: value]
    def new({"_trace", trace}), do: [_trace: trace]

    def new({"d", data}) do
      data
      |> Enum.map(&new(&1))
      |> Enum.concat()
    end

    def new(_), do: []
  end

  @type t :: %__MODULE__{
          heartbeat_interval: integer,
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
