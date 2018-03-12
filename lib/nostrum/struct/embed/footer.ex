defmodule Nostrum.Struct.Embed.Footer do
  @moduledoc """
  Struct representing a Discord embed footer.
  """

  alias Nostrum.Util

  defstruct [
    :text,
    :icon_url,
    :proxy_icon_url
  ]

  @typedoc "Footer text"
  @type text :: String.t | nil

  @typedoc "URL of footer icon"
  @type icon_url :: String.t | nil

  @typedoc "Proxied URL of footer icon"
  @type proxy_icon_url :: String.t | nil

  @type t :: %__MODULE__{
    text: text,
    icon_url: icon_url,
    proxy_icon_url: proxy_icon_url
  }

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
  end
end
