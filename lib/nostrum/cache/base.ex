defmodule Nostrum.Cache.Base do
  # Utilities for nostrum caches.
  @moduledoc since: "0.8.0"
  @moduledoc false

  def mnesia_note,
    do: """
    Please note that this module is only compiled if Mnesia is available on
    your system. See the Mnesia section of the [State](functionality/state.md)
    documentation for more information.

    To retrieve the table name used by this cache, use `table/0`.
    """
end
