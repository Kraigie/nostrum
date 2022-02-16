defmodule Nostrum.Struct.Component.Option do
  @moduledoc """
  Component Options
  """
  @moduledoc since: "0.5"
  alias Nostrum.Struct.Component
  alias Nostrum.Util

  @derive Jason.Encoder
  defstruct [
    :label,
    :value,
    :description,
    :emoji,
    :default
  ]

  @type label :: Component.label()
  @type value :: String.t()
  @type description :: Component.description()
  @type emoji :: Component.emoji()
  @type default :: Component.default()

  @type t :: %__MODULE__{
          default: default,
          description: description,
          emoji: emoji,
          label: label,
          value: value
        }

  @doc false
  @spec to_struct(nil | maybe_improper_list | map) :: Nostrum.Struct.Component.Option.t()
  def to_struct(nil) do
    nil
  end

  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:emoji, nil, &Util.cast(&1, {:struct, Emoji}))

    struct(__MODULE__, new)
  end
end
