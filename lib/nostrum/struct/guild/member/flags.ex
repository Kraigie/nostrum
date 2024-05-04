defmodule Nostrum.Struct.Guild.Member.Flags do
  @moduledoc """
  Struct representing the flags a guild member can have.
  """
  @moduledoc since: "0.9.1"

  import Bitwise

  defstruct did_rejoin: false,
            completed_onboarding: false,
            bypasses_verification: false,
            started_onboarding: false

  @typedoc """
  Member has left and rejoined the guild
  """
  @type did_rejoin :: boolean

  @typedoc """
  Member has completed onboarding
  """
  @type completed_onboarding :: boolean

  @typedoc """
  Member is exempt from guild verification requirements
  """
  @type bypasses_verification :: boolean

  @typedoc """
  Member has started onboarding
  """
  @type started_onboarding :: boolean

  @type flags :: %__MODULE__{
          did_rejoin: did_rejoin,
          completed_onboarding: completed_onboarding,
          bypasses_verification: bypasses_verification,
          started_onboarding: started_onboarding
        }

  @type t :: flags

  @flag_values [
    did_rejoin: 1 <<< 0,
    completed_onboarding: 1 <<< 1,
    bypasses_verification: 1 <<< 2,
    started_onboarding: 1 <<< 3
  ]

  @doc """
  Constructs a flag struct based on an integer from the Discord API, normally from `t:Nostrum.Struct.Guild.Member.flags/0`.

  ## Examples

  ```elixir
  iex> Nostrum.Struct.Guild.Member.Flags.from_integer(9)
  %Nostrum.Struct.Guild.Member.Flags{
    did_rejoin: true,
    completed_onboarding: false,
    bypasses_verification: false,
    started_onboarding: true
  }
  ```
  """
  @spec from_integer(integer()) :: t
  def from_integer(flag_value) do
    boolean_list =
      Enum.map(@flag_values, fn {flag, value} ->
        {flag, (flag_value &&& value) == value}
      end)

    struct(__MODULE__, boolean_list)
  end

  @doc """
  Convert a flag struct to an integer value.

  ## Examples

  ```elixir
  iex> my_flags = %Nostrum.Struct.Guild.Member.Flags{
  ...>   did_rejoin: true,
  ...>   completed_onboarding: false,
  ...>   bypasses_verification: false,
  ...>   started_onboarding: true
  ...> }
  iex> Nostrum.Struct.Guild.Member.Flags.to_integer(my_flags)
  9
  ```
  """
  @spec to_integer(t) :: integer()
  def to_integer(flag_struct) do
    booleans =
      flag_struct
      |> Map.from_struct()
      |> Map.to_list()

    Enum.reduce(booleans, 0, fn {flag, enabled}, flag_value ->
      case enabled do
        true -> flag_value ||| @flag_values[flag]
        false -> flag_value
      end
    end)
  end
end
