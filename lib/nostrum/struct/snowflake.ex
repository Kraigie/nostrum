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
  @type t :: integer

  @doc """
  Returns `true` if `term` is a snowflake; otherwise returns `false`
  """
  defguard is_snowflake(term)
    when is_integer(term)
    and term >= 0
    and term <= 0xFFFFFFFFFFFFFFFF

  @doc """
  Attempts to convert a term into a snowflake.
  """
  @spec cast(term) :: {:ok, t | nil} | :error
  def cast(value)
  def cast(nil), do: {:ok, nil}
  def cast(value) when is_integer(value), do: {:ok, value}

  def cast(value) when is_binary(value) do
    case Integer.parse(value) do
      {snowflake, _} -> {:ok, snowflake}
      _ -> :error
    end
  end

  def cast(_), do: :error

  @doc """
  Same as `cast/1`, except it raises an `ArgumentException` on failure.
  """
  @spec cast!(term) :: t | nil | no_return
  def cast!(value) do
    case cast(value) do
      {:ok, res} -> res
      :error -> raise ArgumentError, "Could not convert to a snowflake"
    end
  end

  @doc """
  Convert a snowflake into its external representation.
  """
  @spec dump(t) :: external_snowflake
  def dump(snowflake) when is_snowflake(snowflake), do: to_string(snowflake)
  def dump(_), do: raise(ArgumentError, "Was not given a snowflake")

  @doc """
  Returns the creation time of the snowflake.
  """
  @spec creation_time(t) :: DateTime.t()
  def creation_time(snowflake) when is_snowflake(snowflake) do
    use Bitwise

    time_elapsed_ms = (snowflake >>> 22) + 1_420_070_400_000

    DateTime.from_unix!(time_elapsed_ms, :milliseconds)
  end
end
