defmodule Nostrum.Struct.Message.Flags do
  @moduledoc """
  Struct representing the flags a message can have
  """
  @moduledoc since: "NEXTVERSION"

  import Bitwise

  defstruct crossposted: false,
            is_crosspost: false,
            suppress_embeds: false,
            source_message_deleted: false,
            urgent: false,
            has_thread: false,
            ephemeral: false,
            loading: false,
            failed_to_mention_some_roles_in_thread: false,
            suppress_notifications: false,
            is_voice_message: false,
            has_snapshot: false,
            is_components_v2: false

  @typedoc """
  Message has been published to subscribed channels (via Channel Following)
  """
  @type crossposted :: boolean

  @typedoc """
  Message originated from a message in another channel (via Channel Following)
  """
  @type is_crosspost :: boolean

  @typedoc """
  Do not include any embeds when serializing message
  """
  @type suppress_embeds :: boolean

  @typedoc """
  The source message for a crosspost has been deleted (via Channel Following)
  """
  @type source_message_deleted :: boolean

  @typedoc """
  Message came from the urgent message system
  """
  @type urgent :: boolean

  @typedoc """
  Message has an associated thread, with the same id as the message
  """
  @type has_thread :: boolean

  @typedoc """
  Message is only visible to the user who invoked the Interaction
  """
  @type ephemeral :: boolean

  @typedoc """
  Message is an Interaction Response and the bot is "thinking"
  """
  @type loading :: boolean

  @typedoc """
  Message failed to mention some roles and add their members to the thread
  """
  @type failed_to_mention_some_roles_in_thread :: boolean

  @typedoc """
  Message will not trigger push and desktop notifications
  """
  @type suppress_notifications :: boolean

  @typedoc """
  Message is a voice message
  """
  @type is_voice_message :: boolean

  @typedoc """
  Message has a snapshot (via Message Forwarding)
  """
  @type has_snapshot :: boolean

  @typedoc """
  Message uses components v2
  """
  @type is_components_v2 :: boolean

  @type flags :: %__MODULE__{
          crossposted: crossposted,
          is_crosspost: is_crosspost,
          suppress_embeds: suppress_embeds,
          source_message_deleted: source_message_deleted,
          urgent: urgent,
          has_thread: has_thread,
          ephemeral: ephemeral,
          loading: loading,
          failed_to_mention_some_roles_in_thread: failed_to_mention_some_roles_in_thread,
          suppress_notifications: suppress_notifications,
          is_voice_message: is_voice_message,
          has_snapshot: has_snapshot,
          is_components_v2: is_components_v2
        }

  @type t :: flags

  @typedoc "Raw message flags as sent by the Discord API"
  @type raw_flags :: integer()

  @flag_values [
    crossposted: 1 <<< 0,
    is_crosspost: 1 <<< 1,
    suppress_embeds: 1 <<< 2,
    source_message_deleted: 1 <<< 3,
    urgent: 1 <<< 4,
    has_thread: 1 <<< 5,
    ephemeral: 1 <<< 6,
    loading: 1 <<< 7,
    failed_to_mention_some_roles_in_thread: 1 <<< 8,
    suppress_notifications: 1 <<< 12,
    is_voice_message: 1 <<< 13,
    has_snapshot: 1 <<< 14,
    is_components_v2: 1 <<< 15
  ]

  @doc """
  Constructs a flag struct based on an integer from the Discord API.

  ## Examples

  ```elixir
  iex> Nostrum.Struct.Message.Flags.from_integer(258)
  %Nostrum.Struct.Message.Flags{
    crossposted: false,
    is_crosspost: true,
    suppress_embeds: false,
    source_message_deleted: false,
    urgent: false,
    has_thread: false,
    ephemeral: false,
    loading: false,
    failed_to_mention_some_roles_in_thread: true,
    suppress_notifications: false,
    is_voice_message: false,
    has_snapshot: false,
    is_components_v2: false
  }
  ```
  """
  @spec from_integer(raw_flags()) :: t
  def from_integer(flag_value) do
    boolean_list =
      Enum.map(@flag_values, fn {flag, value} ->
        {flag, (flag_value &&& value) == value}
      end)

    struct(__MODULE__, boolean_list)
  end

  @doc """
  Convert a flag struct to an integer value.

  ## Examples

  ```elixir
  iex> my_flags = %Nostrum.Struct.Message.Flags{
  ...>  crossposted: false,
  ...>  is_crosspost: true,
  ...>  suppress_embeds: false,
  ...>  source_message_deleted: false,
  ...>  urgent: false,
  ...>  has_thread: false,
  ...>  ephemeral: false,
  ...>  loading: false,
  ...>  failed_to_mention_some_roles_in_thread: true,
  ...>  suppress_notifications: false,
  ...>  is_voice_message: false,
  ...>  has_snapshot: false,
  ...>  is_components_v2: false
  ...> }
  iex> Nostrum.Struct.Message.Flags.to_integer(my_flags)
  258
  ```
  """
  @spec to_integer(t) :: raw_flags()
  def to_integer(flag_struct) do
    booleans =
      flag_struct
      |> Map.from_struct()
      |> Map.to_list()

    Enum.reduce(booleans, 0, fn {flag, enabled}, flag_value ->
      case enabled do
        true -> flag_value ||| @flag_values[flag]
        false -> flag_value
      end
    end)
  end
end
