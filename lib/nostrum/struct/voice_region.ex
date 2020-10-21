defmodule Nostrum.Struct.VoiceRegion do
  @moduledoc """
  Struct representing a Discord voice region.
  """

  @type t :: %{
          custom: boolean,
          deprecated: boolean,
          id: String.t(),
          name: String.t(),
          optimal: boolean,
          vip: boolean
        }
end
