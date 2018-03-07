defmodule Nostrum.Struct.Message.MessageActivity do 
  @moduledoc """ 
  Struct representing a Discord message activity. 
  """

  alias Nostrum.Util

  defstruct [
    :type,
    :party_id
  ]

  @typedoc """
  Type of message activity object.

  * `1` - JOIN
  * `2` - SPECTATE 
  * `3` - LISTEN
  * `5` - JOIN_REQUEST
  """
  @type type :: integer

  @typedoc "The party id from a [rich presence event](https://discordapp.com/developers/docs/rich-presence/how-to)"
  @type party_id :: String.t | nil
 
  @type t :: %__MODULE__{ 
    type: type,
    party_id: party_id
  }
 
  @doc false 
  def to_struct(map) do 
    struct(__MODULE__, Util.safe_atom_map(map))
  end
end 