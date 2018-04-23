defmodule Nostrum.Struct.Snowflake do
  @moduledoc """
  Functions that work on Snowflakes.
  """

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

  ```Elixir
  iex> Nostrum.Struct.Snowflake.is_snowflake(89918932789497856)
  true

  iex> Nostrum.Struct.Snowflake.is_snowflake(-1)
  false

  iex> Nostrum.Struct.Snowflake.is_snowflake(0xFFFFFFFFFFFFFFFF + 1)
  false

  iex> Nostrum.Struct.Snowflake.is_snowflake("117789813427535878")
  false
  ```
  """
  defguard is_snowflake(term)
           when is_integer(term) and term in 0..0xFFFFFFFFFFFFFFFF

  @doc ~S"""
  Attempts to convert a term into a snowflake.

  ## Examples

  ```Elixir
  iex> Nostrum.Struct.Snowflake.cast(200317799350927360)
  {:ok, 200317799350927360}

  iex> Nostrum.Struct.Snowflake.cast("200317799350927360")
  {:ok, 200317799350927360}

  iex> Nostrum.Struct.Snowflake.cast(nil)
  {:ok, nil}

  iex> Nostrum.Struct.Snowflake.cast(true)
  :error

  iex> Nostrum.Struct.Snowflake.cast(-1)
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

  ```Elixir
  iex> Nostrum.Struct.Snowflake.dump(109112383011581952)
  "109112383011581952"
  ```
  """
  @spec dump(t) :: external_snowflake
  def dump(snowflake) when is_snowflake(snowflake), do: to_string(snowflake)

  @doc ~S"""
  Returns the creation time of the snowflake.

  ## Examples

  ```Elixir
  iex> Snowflake.creation_time(177888205536886784)
  #DateTime<2016-05-05 21:04:13.203Z>
  ```
  """
  @spec creation_time(t) :: DateTime.t()
  def creation_time(snowflake) when is_snowflake(snowflake) do
    use Bitwise

    time_elapsed_ms = (snowflake >>> 22) + 1_420_070_400_000

    DateTime.from_unix!(time_elapsed_ms, :milliseconds)
  end
end
