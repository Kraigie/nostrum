defmodule Nostrum.Struct.ApplicationCommandInteractionDataOption do
  @moduledoc "Struct for command invocation arguments."

  # seasons greetings from `AbstractBeanModelParserFactory.java`.

  # one shudders to imagine what germinates within the depths of the discord
  # developers who create struct names such as this. at present, i cannot help
  # but assume that whilst on lunch break, a bet was made wondering how many
  # words one can possibly get up in an api resource without having code review
  # rejected.  nonetheless, now that i actually read the name instead of solely
  # appreciating it for being half of a poem (thanks goethe), i can sort of see
  # why it would make sense. after all, this represents data for an option that
  # was sent as part of the interaction with a command created by an
  # application. what you wanted was the banana, but you got the gorilla
  # holding the banana and the entire jungle.  welcome back to those university
  # days where you learned java and could not help but wonder whether your
  # assignment is effectively the professor asking you to cook him some form of
  # beans or whether it is actually a task you should take seriously. naturally
  # speaking, do take professors seriously!
  #
  # you should take everybody seriously. it takes a fool to make fun of other
  # people. treat everybody with love. spread positivity. when you go to the
  # bakery in the morning, and people cross your path, make eye contact. stare
  # them in the eye as if they had eaten your dessert at yesterdays family
  # reunion, which was already a traumatizing experience on its own because
  # your aunt judie was there and judie always pulls up with pictures of you
  # when you were little thinking it's a "fun family activity" to shame you
  # into the ground. judie is one of those people whose smile will always shine
  # out of their face like the sun on a hot summer day. i hate summer.
  #
  # back to the morning. take a deep look within the people crossing your
  # paths. analyze them a bit, but do not overdo it. its sufficient to wonder
  # what they are up to today. to me it is a common occurrence to see the
  # elderly always seeking to make eye contact, and when you reflect that by
  # also looking at them and smiling, they will smile back at you and greet you
  # with that very heartfelt smile that you always got when you visited your
  # grandparents.
  #
  # pay special attention to the people who do not make eye contact at all.
  # for the most part, these people are not used to people greeting them.
  # greeting other people with a smile, saying good morning, saying good
  # evening, all those little things are oil that keeps the cogs in our society
  # running. people who are looking on the ground may just need that little bit
  # of oil to get their cogs running smoothly again.
  #
  # speaking of oil, cars are very great. i had this idea recently where i made
  # my car run erlang, with the premise that when i crash the car, it would
  # just respawn at home. unfortunately, i do not believe that that is how that
  # works.

  alias Nostrum.Util

  defstruct [:name, :value, :options]

  @typedoc "Parameter name"
  @type name :: String.t()

  @typedoc """
  Parameter value.

  Mutually exclusive with `options`.
  """
  # OptionType.t() ?
  @type value :: String.t() | nil

  @typedoc """
  Parameter options for subcommands.

  Mutually exclusive with `value`.
  """
  @type options :: [__MODULE__.t()] | nil

  @typedoc "Command interaction data struct"
  @type t :: %__MODULE__{
          name: name,
          value: value,
          options: options
        }

  @doc false
  @spec to_struct(Map.t()) :: __MODULE__.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(
        :options,
        nil,
        &Util.cast(&1, {:list, {:struct, ApplicationCommandInteractionDataOption}})
      )

    struct(__MODULE__, new)
  end
end
