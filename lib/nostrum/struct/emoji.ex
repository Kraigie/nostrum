defmodule Nostrum.Struct.Emoji do
  @moduledoc """
  Struct representing a Discord emoji.
  """

  alias Nostrum.Struct.Snowflake
  alias Nostrum.Util

  defstruct [
    :id,
    :name,
    :user,
    :require_colons,
    :managed,
    :animated,
    roles: []
  ]

  @typedoc """ 
  Emoji string to be used with the Discord API. 
 
  Some API endpoints take an `emoji`. This `emoji` string should be structured as `"id:name"`.
 
  Alternatively, the `to_api_name/1` function will convert a `Nostrum.Struct.Emoji` to this string. 
  """ 
  @type emoji_api_name :: String.t

  @typedoc "Id of the emoji"
  @type id :: Snowflake.t | nil

  @typedoc "Name of the emoji"
  @type name :: String.t

  @typedoc "Roles this emoji is whitelisted to"
  @type roles :: [Snowflake.t]

  @typedoc "User that created this emoji"
  @type user :: User.t | nil

  @typedoc "Whether this emoji must be wrapped in colons "
  @type require_colons :: boolean | nil

  @typedoc "Whether this emoji is managed"
  @type managed :: boolean | nil

  @typedoc "Whether this emoji is animated"
  @type animated :: boolean | nil

  @type t :: %__MODULE__{
    id: id,
    name: name,
    roles: roles,
    user: user,
    require_colons: require_colons,
    managed: managed,
    animated: animated
  }
 
  @doc """ 
  Formats an emoji struct into its respective markdown. 
  """ 
  @spec to_markdown(t) :: String.t
  def to_markdown(emoji) 
  def to_markdown(%__MODULE__{id: nil, name: name}), do: name 
  def to_markdown(%__MODULE__{animated: true, id: id, name: name}), do: "<a:#{name}:#{id}>" 
  def to_markdown(%__MODULE__{id: id, name: name}), do: "<:#{name}:#{id}>" 
 
  @doc """ 
  Formats an emoji struct into its api name. 
   
  An emoji's api name is used to represent emojis in Discord API calls. 
  """ 
  @spec to_api_name(t) :: emoji_api_name 
  def to_api_name(emoji)
  def to_api_name(%__MODULE__{id: nil, name: name}), do: name 
  def to_api_name(%__MODULE__{id: id, name: name}), do: "#{name}:#{id}"

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  @doc """
  Formats a custom emoji for use in a Discord message.
  """
  @deprecated "Use `to_markdown/1` instead"
  @spec format_custom_emoji(String.t, String.t | integer) :: String.t
  def format_custom_emoji(name, id) when is_binary(id), do: "<:" <> name <> ":" <> id <> ">"
  def format_custom_emoji(name, id) do
    "<:" <> name <> ":" <> to_string(id) <> ">"
  end

  @doc false
  def to_struct(map) do
    struct(__MODULE__, Util.safe_atom_map(map))
    |> Map.update(:id, nil, &Snowflake.cast!/1)
    |> Map.update(:roles, [], fn roles -> 
      Enum.map(roles, &Snowflake.cast!/1)
    end)
  end
end
