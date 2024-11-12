defmodule Nostrum.Struct.Message.Poll do
  @moduledoc """
  Struct representing a poll in a Discord chat.

  There are various helper methods on this structure to create new poll, see `create_poll/2` and `put_answer/2` & `put_answer/3` for code samples.
  """
  @moduledoc since: "0.9.0"

  alias Nostrum.Util

  alias Nostrum.Snowflake
  alias Nostrum.Struct.Message.Poll.{Answer, MediaObject, Results}

  @derive Jason.Encoder
  defstruct [
    :question,
    :answers,
    :expiry,
    :duration,
    :allow_multiselect,
    :layout_type,
    :results
  ]

  @typedoc """
  Question for the poll
  """
  @type question :: MediaObject.t()

  @typedoc """
  List of potential answers for the poll
  """
  @type answers :: [Answer.t()]

  @typedoc """
  Expiry time of the poll
  """
  @type expiry :: DateTime.t() | nil

  @typedoc """
  Duration of poll in hours
  """
  @type duration :: integer | nil

  @typedoc """
  Whether the poll allows selection of multiple answers
  """
  @type allow_multiselect :: boolean

  @typedoc """
  Layout type for the poll, currently only 1 (`DEFAULT`) is supported here.

  If set to `nil`, the value will default to `1` at the Discord API.
  """
  @type layout_type :: 1 | nil

  @typedoc """
  Result counts of a poll that has been voted on.

  This field is only present for poll objects received over the gateway or Discord API.

  As mentioned in the `Nostrum.Struct.Message.Poll.Results` documentation, if an answer has not been voted on it
  will not be in this object.
  """
  @type results :: Results.t() | nil

  @type t :: %__MODULE__{
          question: question,
          answers: answers,
          expiry: expiry,
          duration: duration,
          allow_multiselect: allow_multiselect,
          layout_type: layout_type,
          results: results
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:expiry, nil, &Util.maybe_to_datetime/1)
      |> Map.update(:question, nil, &Util.cast(&1, {:struct, MediaObject}))
      |> Map.update(:answers, nil, &Util.cast(&1, {:list, {:struct, Answer}}))
      |> Map.update(:results, nil, &Util.cast(&1, {:struct, Results}))

    struct(__MODULE__, new)
  end

  @doc ~S"""
  Create a new poll struct.

  Use `Nostrum.Api.Message.create/2` to send it once you've populated it.

  Accepts a `question_text` parameter which is the string to use as the poll title.

  Keyword arguments:
  - `duration`: duration (in hours) the poll should be open for
  - `allow_multiselect`: whether users should be able to select multiple answers

  You can also pass an `answers` key with answers, though `put_answer/2` and `put_answer/3` are advised.

  ## Examples

  ```elixir
  poll = Poll.create_poll("Do you enjoy pineapple on pizza?", duration: 2, allow_multiselect: false)
          |> Poll.put_answer("Yes!", default_emoji: "\u2705") # check mark emoji
          |> Poll.put_answer("No!", default_emoji: "\u274C") # cross emoji

  Nostrum.Api.Message.create(channel_id, poll: poll)
  ```
  """
  @spec create_poll(String.t(), duration: duration, allow_multiselect: allow_multiselect) :: t()
  def create_poll(question_text, duration: duration, allow_multiselect: allow_multiselect) do
    %__MODULE__{
      question: %MediaObject{
        text: question_text
      },
      answers: [],
      duration: duration,
      allow_multiselect: allow_multiselect,
      layout_type: 1
    }
  end

  @spec create_poll(String.t(),
          duration: duration,
          allow_multiselect: allow_multiselect,
          answers: [Answer.t()]
        ) :: t()
  def create_poll(question_text,
        duration: duration,
        allow_multiselect: allow_multiselect,
        answers: answers
      ) do
    poll = create_poll(question_text, duration: duration, allow_multiselect: allow_multiselect)

    Map.put(poll, :answers, answers)
  end

  defp add_answer(poll, answer) do
    Map.update(poll, :answers, [answer], fn answers -> answers ++ [answer] end)
  end

  @doc ~S"""
  Add an answer to the provided poll.

  See `create_poll/2` for a code sample of this function.

  Takes a required "answer" text field, as well as either of the optional arguments:
  - `custom_emoji`: An integer representing the snowflake of an emoji to display with the option
  - `default_emoji`: A default platform emoji represented as a unicode character
  """
  @spec put_answer(t(), String.t()) :: t()
  def put_answer(poll, answer) do
    new_answer = %Answer{
      poll_media: %MediaObject{
        text: answer
      }
    }

    add_answer(poll, new_answer)
  end

  @spec put_answer(t(), String.t(), custom_emoji: Snowflake.t()) :: t()
  def put_answer(poll, answer, custom_emoji: custom_emoji) do
    new_answer = %Answer{
      poll_media: %MediaObject{
        text: answer,
        emoji: %{
          id: custom_emoji
        }
      }
    }

    add_answer(poll, new_answer)
  end

  @spec put_answer(t(), String.t(), default_emoji: String.t()) :: t()
  def put_answer(poll, answer, default_emoji: default_emoji) do
    new_answer = %Answer{
      poll_media: %MediaObject{
        text: answer,
        emoji: %{
          name: default_emoji
        }
      }
    }

    add_answer(poll, new_answer)
  end
end
