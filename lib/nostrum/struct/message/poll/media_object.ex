defmodule Nostrum.Struct.Message.Poll.MediaObject do
  @moduledoc """
  A struct representing a media item of a poll (e.g. a question or answer)
  """

  alias Nostrum.Util

  @derive Jason.Encoder
  defstruct [
    :text,
    :emoji
  ]

  @typedoc """
  Text of the poll media object, either the question or answer text.
  """
  @type text :: String.t() | nil

  @typedoc """
  A partial emoji (only supported for answers).

  For a custom emoji, only the `id` field needs to be sent, for a default emoji, only the
  `name` field needs to be sent (with the Unicode emoji).
  """
  @type emoji :: %{id: integer | nil, name: String.t() | nil}

  @type t :: %__MODULE__{
          text: text,
          emoji: emoji
        }

  @doc false
  def to_struct(map) do
    new = Map.new(map, fn {k, v} -> {Util.maybe_to_atom(k), v} end)

    struct(__MODULE__, new)
  end
end
