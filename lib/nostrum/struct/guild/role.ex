defmodule Nostrum.Struct.Guild.Role do
  @moduledoc ~S"""
  Struct representing a Discord role.

  ## Mentioning Roles in Messages

  A `Nostrum.Struct.Guild.Role` can be mentioned in message content using the `String.Chars`
  protocol or `mention/1`.

  ```elixir
  role = %Nostrum.Struct.Guild.Role{id: 431886897539973131}
  Nostrum.Api.create_message!(184046599834435585, "#{role}")
  %Nostrum.Struct.Message{}

  role = %Nostrum.Struct.Guild.Role{id: 431884023535632398}
  Nostrum.Api.create_message!(280085880452939778, "#{Nostrum.Struct.Guild.Role.mention(role)}")
  %Nostrum.Struct.Message{}
  ```
  """

  alias Nostrum.{Snowflake, Util}

  defstruct [
    :id,
    :name,
    :color,
    :hoist,
    :position,
    :permissions,
    :managed,
    :mentionable,
    :icon,
    :unicode_emoji
  ]

  defimpl String.Chars do
    def to_string(role), do: @for.mention(role)
  end

  @typedoc "The id of the role"
  @type id :: Snowflake.t()

  @typedoc "The name of the role"
  @type name :: String.t()

  @typedoc "The hexadecimal color code"
  @type color :: integer

  @typedoc "Whether the role is pinned in the user listing"
  @type hoist :: boolean

  @typedoc "The position of the role"
  @type position :: integer

  @typedoc "The permission bit set"
  @type permissions :: integer

  @typedoc "Whether the role is managed by an integration"
  @type managed :: boolean

  @typedoc "Whether the role is mentionable"
  @type mentionable :: boolean

  @typedoc "The hash of the role icon"
  @typedoc since: "0.7.0"
  @type icon :: String.t() | nil

  @typedoc "The standard unicode character emoji icon for the role"
  @typedoc since: "0.7.0"
  @type unicode_emoji :: String.t() | nil

  @type t :: %__MODULE__{
          id: id,
          name: name,
          color: color,
          hoist: hoist,
          position: position,
          permissions: permissions,
          managed: managed,
          mentionable: mentionable,
          icon: icon,
          unicode_emoji: unicode_emoji
        }

  @doc ~S"""
  Formats an `Nostrum.Struct.Role` into a mention.

  ## Examples

  ```elixir
  iex> role = %Nostrum.Struct.Guild.Role{id: 431886639627763722}
  ...> Nostrum.Struct.Guild.Role.mention(role)
  "<@&431886639627763722>"
  ```
  """
  @spec mention(t) :: String.t()
  def mention(%__MODULE__{id: id}), do: "<@&#{id}>"

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
      |> Map.update(:permissions, nil, fn
        perm when is_binary(perm) -> String.to_integer(perm)
        x -> x
      end)

    struct(__MODULE__, new)
  end
end
