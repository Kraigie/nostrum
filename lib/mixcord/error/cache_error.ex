defmodule Mixcord.Error.CacheError do
  @moduledoc """
  Represents an error when interacting with the cache.
  """

  defexception [:message]

  def exception(looking_for, cache_name) do
    msg = "ERROR: No match for #{inspect looking_for} found in #{cache_name}"
    %__MODULE__{message: msg}
  end

end