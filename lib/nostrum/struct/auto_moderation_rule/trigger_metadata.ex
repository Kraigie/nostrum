defmodule Nostrum.Struct.AutoModerationRule.TriggerMetadata do
  @moduledoc """
  Struct representing the metadata of a trigger.
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Util

  defstruct [
    :keyword_filter,
    :presets
  ]

  @typedoc """
  A list of Values which represent the different presets defined by Discord

  | value | type | description
  | ---- | ---- | -----------
  | `1` | `PROFANITY` | Words which may be considered profane
  | `2` | `HARMFUL_LINK` | Words that refer to sexually explicit behavior or activity
  | `3` | `SLURS` | Personal insults or words that may be considered hate speech
  """
  @type preset_value_metadata :: %__MODULE__{
          presets: [1..3]
        }

  @typedoc """
  Contains the list of keywords to that will trigger the rule.
  """
  @type keyword_metadata :: %__MODULE__{
          keyword_filter: [String.t()]
        }

  @typedoc """
  Additional data used to determine if the rule should triggered.

  The `t:Nostrum.Struct.AutoModerationRule.trigger_type/0` of the parent struct determine which of the following fields are not `nil`.

  | key | associated `trigger_type`
  | ---- | -----------
  | `keywords` | `​KEYWORD`
  | `preset` | `KEYWORD_PRESET`
  """
  @type t :: preset_value_metadata() | keyword_metadata()

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
