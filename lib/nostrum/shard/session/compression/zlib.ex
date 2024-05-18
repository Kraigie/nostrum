defmodule Nostrum.Shard.Session.Compression.Zlib do
  @moduledoc """
  Implementation of compression methods for the `zlib` compression algorithm.

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and people curious to look behind the covers.
  """

  @behaviour Nostrum.Shard.Session.Compression

  @spec create_context() :: :zlib.zstream()
  def create_context do
    context = :zlib.open()
    :zlib.inflateInit(context)

    context
  end

  @spec inflate(:zlib.zstream(), iodata()) :: iolist()
  def inflate(ctx, frame) do
    :zlib.inflate(ctx, frame)
  end

  @spec reset_context(:zlib.zstream()) :: :zlib.zstream()
  def reset_context(ctx) do
    :zlib.inflateReset(ctx)

    ctx
  end
end
