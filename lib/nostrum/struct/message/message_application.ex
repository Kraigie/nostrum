defmodule Nostrum.Struct.Message.MessageApplication do 
  @moduledoc """ 
  Struct representing a Discord message application. 
  """ 
 
  alias Nostrum.Struct.Snowflake
  alias Nostrum.Util
 
  defstruct [
    :id,
    :cover_image,
    :description,
    :icon,
    :name
  ]
  
  @typedoc "Id of the application"
  @type id :: Snowflake.t

  @typedoc "Id of the embed's image asset"
  @type cover_image :: String.t

  @typedoc "Application's description"
  @type description :: String.t

  @typedoc "Id of the application's icon"
  @type icon :: String.t

  @typedoc "Name of the application"
  @type name :: String.t
 
  @type t :: %__MODULE__{ 
    id: id, 
    cover_image: cover_image, 
    description: description, 
    icon: icon, 
    name: name
  }
 
  @doc false 
  def to_struct(map) do 
    struct(__MODULE__, Util.safe_atom_map(map))
    |> Map.update(:id, nil, &Snowflake.cast!/1)
  end
end