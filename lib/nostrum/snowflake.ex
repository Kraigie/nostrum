defmodule Nostrum.Snowflake do
  @moduledoc """
  Functions that work on Snowflakes.
  """

  alias Nostrum.Constants
  import Bitwise

  @typedoc """
  The type that represents snowflakes in JSON.

  In JSON, Snowflakes are typically represented as strings due
  to some languages not being able to represent such a large number.
  """
  @type external_snowflake :: String.t()

  @typedoc """
  The snowflake type.

  Snowflakes are 64-bit unsigned integers used to represent discord
  object ids.
  """
  @type t :: 0..0xFFFFFFFFFFFFFFFF

  @doc ~S"""
  Returns `true` if `term` is a snowflake; otherwise returns `false`.

  ## Examples

  ```elixir
  iex> Nostrum.Snowflake.is_snowflake(89918932789497856)
  true

  iex> Nostrum.Snowflake.is_snowflake(-1)
  false

  iex> Nostrum.Snowflake.is_snowflake(0xFFFFFFFFFFFFFFFF + 1)
  false

  iex> Nostrum.Snowflake.is_snowflake("117789813427535878")
  false
  ```
  """
  defguard is_snowflake(term)
           when is_integer(term) and term in 0..0xFFFFFFFFFFFFFFFF

  @doc ~S"""
  Attempts to convert a term into a snowflake.

  ## Examples

  ```elixir
  iex> Nostrum.Snowflake.cast(200317799350927360)
  {:ok, 200317799350927360}

  iex> Nostrum.Snowflake.cast("200317799350927360")
  {:ok, 200317799350927360}

  iex> Nostrum.Snowflake.cast(nil)
  {:ok, nil}

  iex> Nostrum.Snowflake.cast(true)
  :error

  iex> Nostrum.Snowflake.cast(-1)
  :error
  ```
  """
  @spec cast(term) :: {:ok, t | nil} | :error
  def cast(value)
  def cast(nil), do: {:ok, nil}
  def cast(value) when is_snowflake(value), do: {:ok, value}

  def cast(value) when is_binary(value) do
    case Integer.parse(value) do
      {snowflake, _} -> cast(snowflake)
      _ -> :error
    end
  end

  def cast(_), do: :error

  @doc """
  Same as `cast/1`, except it raises an `ArgumentError` on failure.
  """
  @spec cast!(term) :: t | nil | no_return
  def cast!(value) do
    case cast(value) do
      {:ok, res} -> res
      :error -> raise ArgumentError, "Could not convert to a snowflake"
    end
  end

  @doc ~S"""
  Convert a snowflake into its external representation.

  ## Examples

  ```elixir
  iex> Nostrum.Snowflake.dump(109112383011581952)
  "109112383011581952"
  ```
  """
  @spec dump(t) :: external_snowflake
  def dump(snowflake) when is_snowflake(snowflake), do: to_string(snowflake)

  @doc """
  Converts the given `datetime` into a snowflake.

  If `datetime` occurred before the discord epoch, the function will return
  `:error`.

  The converted snowflake's last 22 bits will be zeroed out due to missing data.

  ## Examples

  ```elixir
  iex> {:ok, dt, _} = DateTime.from_iso8601("2016-05-05T21:04:13.203Z")
  iex> Nostrum.Snowflake.from_datetime(dt)
  {:ok, 177888205536755712}

  iex> {:ok, dt, _} = DateTime.from_iso8601("1998-12-25T00:00:00.000Z")
  iex> Nostrum.Snowflake.from_datetime(dt)
  :error
  ```
  """
  @spec from_datetime(DateTime.t()) :: {:ok, t} | :error
  def from_datetime(%DateTime{} = datetime) do
    unix_time_ms = DateTime.to_unix(datetime, :millisecond)
    discord_time_ms = unix_time_ms - Constants.discord_epoch()

    if discord_time_ms >= 0 do
      {:ok, discord_time_ms <<< 22}
    else
      :error
    end
  end

  @doc """
  Same as `from_datetime/1`, except it raises an `ArgumentError` on failure.
  """
  @spec from_datetime!(DateTime.t()) :: t | no_return
  def from_datetime!(datetime) do
    case from_datetime(datetime) do
      {:ok, snowflake} -> snowflake
      :error -> raise(ArgumentError, "invalid datetime #{inspect(datetime)}")
    end
  end

  @doc ~S"""
  Returns the creation time of the snowflake.

  ## Examples

  ```elixir
  iex> Nostrum.Snowflake.creation_time(177888205536886784)
  ~U[2016-05-05 21:04:13.203Z]
  ```
  """
  @spec creation_time(t) :: DateTime.t()
  def creation_time(snowflake) when is_snowflake(snowflake) do
    time_elapsed_ms = (snowflake >>> 22) + Constants.discord_epoch()

    {:ok, datetime} = DateTime.from_unix(time_elapsed_ms, :millisecond)
    datetime
  end
end
