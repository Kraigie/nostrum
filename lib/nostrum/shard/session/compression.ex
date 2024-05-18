defmodule Nostrum.Shard.Session.Compression do
  @moduledoc """
  A behaviour for compression methods supported by the Discord gateway to implement.

  See the modules nested under this behaviour for reference implementations.

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and people curious to look behind the covers.
  """

  @doc """
  Create a new compression context that can be passed as an argument to other
  methods within the behaviour to inflate data or reset the context to a
  blank state.
  """
  @callback create_context() :: reference()

  @doc """
  Decompress a frame received from Discord over the gateway. Should return an
  iolist of the decompressed data.
  """
  @callback inflate(reference(), iodata()) :: iolist()

  @doc """
  Reset a decompression context to a blank slate, this is useful after a websocket
  resume has taken place or something similar requiring the reset of the state
  for a shard.
  """
  @callback reset_context(reference()) :: reference()
end
