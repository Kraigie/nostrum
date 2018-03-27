defmodule Nostrum.Error.CacheError do
  @moduledoc """
  Represents an error when interacting with the cache.

  This likely occurs because a specified item could not be found in the cache,
  or your were searching for something invalid.
  This should only occur when using the banged cache methods.
  """

  defexception [:message]

  def exception(finding: finding, cache_name: cache_name) do
    msg = "ERROR: No match for #{inspect(finding)} found in #{cache_name}"
    %__MODULE__{message: msg}
  end

  def exception(key: key, cache_name: cache_name) do
    msg = "ERROR: Key #{inspect(key)} not found in #{cache_name}"
    %__MODULE__{message: msg}
  end

  def exception(msg) do
    %__MODULE__{message: msg}
  end
end
