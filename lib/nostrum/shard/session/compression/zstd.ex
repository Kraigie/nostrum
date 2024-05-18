defmodule Nostrum.Shard.Session.Compression.Zstd do
  @moduledoc """
  Implementation of compression methods for the `zstd` compression algorithm.

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and people curious to look behind the covers.
  """

  @behaviour Nostrum.Shard.Session.Compression

  def check_available! do
    if not Code.ensure_loaded?(:ezstd) do
      zstd_missing()
    end
  end

  @spec zstd_missing() :: no_return
  defp zstd_missing do
    raise ArgumentError, """
    Cannot use the :zstd gateway compression option without optional dependency :ezstd.

    See https://kraigie.github.io/nostrum/gateway_compression.html for more information.
    """
  end

  if Code.ensure_loaded?(:ezstd) do
    @zstd_buffer_size 16_000

    @spec create_context() :: reference() | {:error, any()}
    def create_context do
      :ezstd.create_decompression_context(@zstd_buffer_size)
    end

    @spec inflate(reference(), iodata()) :: iolist() | {:error, any()}
    def inflate(ctx, frame) do
      :ezstd.decompress_streaming(ctx, frame)
    end

    @spec reset_context(reference()) :: reference()
    def reset_context(_ctx) do
      create_context()
    end
  else
    @spec create_context() :: reference() | {:error, any()}
    def create_context do
      zstd_missing()
    end

    @spec inflate(reference(), iodata()) :: iodata() | {:error, any()}
    def inflate(_ctx, _frame) do
      zstd_missing()
    end

    @spec reset_context(reference()) :: reference()
    def reset_context(_ctx) do
      zstd_missing()
    end
  end
end
