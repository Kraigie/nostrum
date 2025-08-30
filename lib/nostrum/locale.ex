defmodule Nostrum.Locale do
  @moduledoc """
  Functions related to locales supported in localizations.

  Currently, these are used in description and name localization within application commands.
  """

  @locales [
    :id,
    :da,
    :de,
    :en_gb,
    :en_us,
    :es_es,
    :es_419,
    :fr,
    :hr,
    :it,
    :lt,
    :hu,
    :nl,
    :no,
    :pl,
    :pt_br,
    :ro,
    :fi,
    :sv_se,
    :vi,
    :tr,
    :cs,
    :el,
    :bg,
    :ru,
    :uk,
    :hi,
    :th,
    :zh_cn,
    :ja,
    :zh_tw,
    :ko
  ]

  # HACK
  # This seems super hacky, but I don't want to maintain a duplicated list & type.
  # We're basically just taking a list and turning it into a union instead.
  @type t :: @locales |> Enum.reduce(&{:|, [], [&1,&2]}) |> unquote()

  @doc """
  Returns `true` if `term` is a locale; otherwise returns `false`.

  ##694

  ```elixir
  iex> Nostrum.Locale.is_locale(:en_us)
  true

  iex> Nostrum.Locale.is_locale(:not_a_locale)
  false
  ```
  """
  defguard is_locale(term) when is_atom(term) and term in @locales

  @doc """
  Returns a list of all locales.
  """
  @spec all() :: [t]
  def all, do: @locales

  @doc """
  Converts the given locale atom (or list of locale atoms) to strings formatted for API use.
  """

end
