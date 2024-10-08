defmodule Nostrum.Struct.Component.DefaultValue do
  @moduledoc """
  Default Value
  """

  @derive Jason.Encoder
  defstruct [
    :id,
    :type
  ]

  @type t :: %__MODULE__{
          id: id(),
          type: type()
        }

  @type id :: Nostrum.Snowflake.t()
  @type type :: :user | :role | :channel
end
