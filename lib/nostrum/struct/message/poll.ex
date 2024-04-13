defmodule Nostrum.Struct.Message.Poll do
  @moduledoc """
  Struct representing a poll object in a Discord chat.
  """

  alias Nostrum.Util

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
  Whether the poll allows selection of multiple ansewrs
  """
  @type allow_multiselect :: boolean

  @typedoc """
  Layout type for the poll, currently only 1 (DEFAULT) is supported here.
  """
  @type layout_type :: 1 | nil

  @typedoc """
  Results of the poll, assuming the poll is from Discord. Empty for a newly created Poll structure.
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

  def put_answer(poll, answer) do
    new_answer = %Answer{
      poll_media: %MediaObject{
        text: answer
      }
    }

    add_answer(poll, new_answer)
  end

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
