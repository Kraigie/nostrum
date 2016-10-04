defmodule Mixcord.Errors.CacheError do
  defexception [:message]

  def exception() do
    msg = "ERROR: No matching item found in the cache"
    %__MODULE__{message: msg}
  end

end