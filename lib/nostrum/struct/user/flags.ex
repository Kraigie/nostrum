defmodule Nostrum.Struct.User.Flags do
  @moduledoc """
  Struct representing the flags a user account can have
  """

  import Bitwise

  defstruct staff: false,
            partner: false,
            hypesquad_events: false,
            bug_hunter_level_1: false,
            hypesquad_bravery: false,
            hypesquad_brilliance: false,
            hypesquad_balance: false,
            early_supporter: false,
            team_user: false,
            system: false,
            bug_hunter_level_2: false,
            verified_bot: false,
            verified_developer: false

  @typedoc """
  Discord Employee
  """
  @type staff :: boolean

  @typedoc """
  Discord Partner
  """
  @type partner :: boolean

  @typedoc """
  HypeSquad Events
  """
  @type hypesquad_events :: boolean

  @typedoc """
  Bug Hunter (Level 1)
  """
  @type bug_hunter_level_1 :: boolean

  @typedoc """
  HypeSquad Bravery
  """
  @type hypesquad_bravery :: boolean

  @typedoc """
  HypeSquad Brilliance
  """
  @type hypesquad_brilliance :: boolean

  @typedoc """
  HypeSquad Balance
  """
  @type hypesquad_balance :: boolean

  @typedoc """
  Early Supporter
  """
  @type early_supporter :: boolean

  @typedoc """
  Team User
  """
  @type team_user :: boolean

  @typedoc """
  System user
  """
  @type system :: boolean

  @typedoc """
  Bug Hunter (Level 2)
  """
  @type bug_hunter_level_2 :: boolean

  @typedoc """
  Verified bot
  """
  @type verified_bot :: boolean

  @typedoc """
  Verified developer
  """
  @type verified_developer :: boolean

  @type flags :: %__MODULE__{
          staff: staff,
          partner: partner,
          hypesquad_events: hypesquad_events,
          bug_hunter_level_1: bug_hunter_level_1,
          hypesquad_bravery: hypesquad_bravery,
          hypesquad_brilliance: hypesquad_brilliance,
          hypesquad_balance: hypesquad_balance,
          early_supporter: early_supporter,
          team_user: team_user,
          system: system,
          bug_hunter_level_2: bug_hunter_level_2,
          verified_bot: verified_bot,
          verified_developer: verified_developer
        }

  @type t :: flags

  @typedoc "Raw user flags as sent by the Discord API"
  @type raw_flags :: integer()

  @flag_values [
    staff: 1 <<< 0,
    partner: 1 <<< 1,
    hypesquad_events: 1 <<< 2,
    bug_hunter_level_1: 1 <<< 3,
    hypesquad_bravery: 1 <<< 6,
    hypesquad_brilliance: 1 <<< 7,
    hypesquad_balance: 1 <<< 8,
    early_supporter: 1 <<< 9,
    team_user: 1 <<< 10,
    system: 1 <<< 12,
    bug_hunter_level_2: 1 <<< 14,
    verified_bot: 1 <<< 16,
    verified_developer: 1 <<< 17
  ]

  @doc """
  Constructs a flag struct based on an integer from the Discord API (either public_flags or flags).

  ## Examples

  ```elixir
  iex> Nostrum.Struct.User.Flags.from_integer(131842)
  %Nostrum.Struct.User.Flags{
    bug_hunter_level_1: false,
    bug_hunter_level_2: false,
    early_supporter: true,
    hypesquad_balance: true,
    hypesquad_bravery: false,
    hypesquad_brilliance: false,
    hypesquad_events: false,
    partner: true,
    staff: false,
    system: false,
    team_user: false,
    verified_bot: false,
    verified_developer: true
  }
  ```
  """
  @spec from_integer(raw_flags()) :: t
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
  iex> my_flags = %Nostrum.Struct.User.Flags{
  ...>  bug_hunter_level_1: false,
  ...>  bug_hunter_level_2: false,
  ...>  early_supporter: true,
  ...>  hypesquad_balance: true,
  ...>  hypesquad_bravery: false,
  ...>  hypesquad_brilliance: false,
  ...>  hypesquad_events: false,
  ...>  partner: true,
  ...>  staff: false,
  ...>  system: false,
  ...>  team_user: false,
  ...>  verified_bot: false,
  ...>  verified_developer: true
  ...> }
  iex> Nostrum.Struct.User.Flags.to_integer(my_flags)
  131842
  ```
  """
  @spec to_integer(t) :: raw_flags()
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
