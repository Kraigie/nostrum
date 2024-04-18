defmodule Nostrum.Struct.Message.Poll.Answer do
  @moduledoc """
  A struct representing a poll answer.
  """

  alias Nostrum.Util

  alias Nostrum.Struct.Message.Poll.MediaObject

  @derive Jason.Encoder
  defstruct [
    :answer_id,
    :poll_media
  ]

  @typedoc """
  ID of the answer, this is only sent *from* the gateway, you do not need to send this to the gateway.
  """
  @type answer_id :: integer | nil

  @typedoc """
  Object representing how the answer is displayed visually, with the text and optional emojis.
  """
  @type poll_media :: MediaObject.t()

  @type t :: %__MODULE__{
          answer_id: answer_id,
          poll_media: poll_media
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:poll_media, nil, &Util.cast(&1, {:struct, MediaObject}))

    struct(__MODULE__, new)
  end
end
