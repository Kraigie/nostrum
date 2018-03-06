defmodule Nostrum.Struct.Snowflake do
  @moduledoc """
  Functions that work on Snowflakes.

  Snowflakes are 64-bit unsigned integers used to represent discord 
  object ids. In JSON, they are typically represented as strings due 
  to some languages not being able to represent such a large number.
  
  Nostrum represents snowflakes as integers since Elixir allows us 
  to represent large integers.
  """

  @type external_snowflake :: String.t

  @type t :: integer

  @doc """
  Returns `true` if `term` is a snowflake; otherwise returns `false`
  """
  defmacro is_snowflake(term) do
    quote do
      is_integer(unquote(term))
    end
  end

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
  def dump(_), do: raise ArgumentError, "Was not given a snowflake"

  
end