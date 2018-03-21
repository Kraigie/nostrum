defmodule Nostrum.Struct.Emoji do
  @moduledoc ~S"""
  Struct representing a Discord emoji.

  ## Using Emojis in Messages

  A `Nostrum.Struct.Emoji` can be used in message content using the `String.Chars` 
  protocol or `format_mention/1`. We highly recommend using the `String.Chars` protocol 
  instead of the latter.

  ```Elixir
  emoji = %Nostrum.Struct.Emoji{id: 43819043108, name: "foxbot"}

  Nostrum.Api.create_message!(189098431762321, "#{emoji}")
  Nostrum.Api.create_message!(189098431762321, "#{Nostrum.Struct.Emoji.format_mention(emoji)}")
  ```

  ## Using Emojis in the Api

  See `t:Nostrum.Struct.Emoji.emoji_api_name/0`.
  """

  alias Nostrum.Struct.Snowflake
  alias Nostrum.Struct.User
  alias Nostrum.Util

  defstruct [
    :id,
    :name,
    :user,
    :require_colons,
    :managed,
    :animated,
    :roles
  ]

  @typedoc ~S"""
  Emoji string to be used with the Discord API.

  Some API endpoints take an `emoji`. If it is a custom emoji, it must be 
  structured as `"id:name"`. If it is an unicode emoji, it can be structured 
  as any of the following: 

    * `"name"`
    * A base 16 unicode emoji string.
    * A URI encoded string.

  We suggest library users to use the `get_api_name/1` function to get a 
  `Nostrum.Struct.Emoji`'s emoji api name.

  ## Examples

  ```Elixir
  # Custom Emojis
  "nostrum:431890438091489"

  # Unicode Emojis
  "≡ƒæì"
  "\xF0\x9F\x98\x81"
  URI.encode("\u2b50")

  # All Emojis
  emoji = %Nostrum.Struct.Emoji{id: 43819043108, name: "foxbot"}
  Nostrum.Struct.Emoji.get_api_name(emoji)
  "foxbot:43819043108"
  ```

  """
  @type emoji_api_name :: String.t

  @typedoc "Id of the emoji"
  @type id :: Snowflake.t | nil

  @typedoc "Name of the emoji"
  @type name :: String.t

  @typedoc "Roles this emoji is whitelisted to"
  @type roles :: [Snowflake.t] | nil

  @typedoc "User that created this emoji"
  @type user :: User.t | nil

  @typedoc "Whether this emoji must be wrapped in colons"
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

  @doc ~S"""
  Formats an emoji struct into an emoji mention.

  Because `Nostrum.Struct.Emoji` implements the `String.Chars` protocol, we 
  recommend nostrum users to use `String.Chars` instead of this function.
  See `Nostrum.Struct.Emoji` for more information.

  ## Examples

  ```Elixir
  emoji = %Nostrum.Struct.Emoji{id: 43819043108, name: "foxbot"}

  Nostrum.Struct.Emoji.format_mention(emoji)
  "<:foxbot:43819043108>"

  Nostrum.Api.create_message!(4318940318049, "Sending some text with this emoji #{Nostrum.Struct.Emoji.format_mention(emoji)}")
  ```
  """
  @spec format_mention(t) :: String.t
  def format_mention(emoji)
  def format_mention(%__MODULE__{id: nil, name: name}), do: name
  def format_mention(%__MODULE__{animated: true, id: id, name: name}), do: "<a:#{name}:#{id}>"
  def format_mention(%__MODULE__{id: id, name: name}), do: "<:#{name}:#{id}>"

  @doc ~S"""
  Formats an emoji struct into its `t:Nostrum.Struct.Emoji.emoji_api_name/0`.

  ## Examples

  ```Elixir
  emoji = %Nostrum.Struct.Emoji{id: 43819043108, name: "foxbot"}

  api_name = Nostrum.Struct.Emoji.get_api_name(emoji)
  "foxbot:43819043108"

  Nostrum.Api.create_reaction(471635274857, 47361625345554, api_name)
  {:ok}
  ```
  """
  @spec get_api_name(t) :: emoji_api_name
  def get_api_name(emoji)
  def get_api_name(%__MODULE__{id: nil, name: name}), do: name
  def get_api_name(%__MODULE__{id: id, name: name}), do: "#{name}:#{id}"

  @doc false
  def p_encode do
    %__MODULE__{}
  end

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:roles, nil, &Util.cast(&1, {:list, Snowflake}))
      |> Map.update(:user, nil, &Util.cast(&1, {:struct, User}))

    struct(__MODULE__, new)
  end
end

defimpl String.Chars, for: Nostrum.Struct.Emoji do
  alias Nostrum.Struct.Emoji

  def to_string(emoji) do
    Emoji.format_mention(emoji)
  end
end
