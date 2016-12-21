defmodule Mixcord.Error.CacheError do
  @moduledoc """
  Represents an error when interacting with the cache.
  """

  defexception [:message]

  def exception(finding: finding, cache_name: cache_name) do
    msg = "ERROR: No match for #{inspect finding} found in #{cache_name}"
    %__MODULE__{message: msg}
  end

end