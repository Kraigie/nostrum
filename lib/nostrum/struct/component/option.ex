defmodule Nostrum.Struct.Component.Option do
  @moduledoc """
  Component Options
  """
  alias Nostrum.Struct.Component
  alias Nostrum.Util

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

  def to_struct(map) do
    %__MODULE__{
      label: map["label"],
      value: map["value"],
      description: map["description"],
      emoji: Util.cast(map["emoji"], {:struct, Emoji}),
      default: map["default"]
    }
  end
end
