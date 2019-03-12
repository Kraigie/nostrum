defmodule Nostrum.Struct.Message.Attachment do
  @moduledoc """
  Struct representing a Discord message attachment.
  """

  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :filename,
    :size,
    :url,
    :proxy_url,
    :height,
    :width
  ]

  @typedoc "Attachment id"
  @type id :: Snowflake.t()

  @typedoc "Name of attached file"
  @type filename :: String.t()

  @typedoc "Size of the file in bytes"
  @type size :: integer

  @typedoc "Source url of the file"
  @type url :: String.t()

  @typedoc "Proxy url of the file"
  @type proxy_url :: String.t()

  @typedoc "Height of the file (if image)"
  @type height :: integer | nil

  @typedoc "Width of the file (if image)"
  @type width :: integer | nil

  @type t :: %__MODULE__{
          id: id,
          filename: filename,
          size: size,
          url: url,
          proxy_url: proxy_url,
          height: height,
          width: width
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
