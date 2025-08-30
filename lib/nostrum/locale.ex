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

  # HACK To avoid maintaining a duplicate list & type. Builds a union via AST from @locales.
  @type t ::
          unquote(
            Enum.reduce(
              @locales,
              fn locale, acc ->
                {:|, [], [locale, acc]}
              end
            )
          )

  @doc """
  Returns `true` if `term` is a locale; otherwise returns `false`.

  ## Examples

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
  Converts the given locale atom to strings formatted for Discord API use.

  ## Examples

  ```elixir
  iex> Nostrum.Locale.to_formatted_string(:en_us)
  "en-US"
  ```
  """
  @spec to_formatted_string(t) :: String.t()
  def to_formatted_string(locale) when is_locale(locale) do
    [head | tail] =
      locale
      |> Atom.to_string()
      |> String.split("_")

    [head | Enum.map(tail, &String.upcase/1)]
    |> Enum.join("-")
  end
end
