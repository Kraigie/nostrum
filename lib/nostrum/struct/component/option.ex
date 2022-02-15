defmodule Nostrum.Struct.Component.Option do
  @moduledoc """
  Component Options
  """
  @moduledoc since: "0.5.0"
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

  @typedoc """
  The user-facing name of the option.
  """
  @type label :: Component.label()

  @typedoc """
  The developer defined value of the option.
  """
  @type value :: String.t()

  @typedoc """
  An optional description of the option. Max length is 100 characters.
  """
  @type description :: String.t() | nil
  @type emoji :: Component.emoji()

  @typedoc """
  If this option is selected by default.
  """
  @type default :: boolean() | nil

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
