defmodule Nostrum.Struct.Guild.SystemChannelFlags do
  @moduledoc """
  Struct representing the flags on a guild's system channel
  """

  import Bitwise

  defstruct suppress_join_notifications: false,
            suppress_premium_subscriptions: false,
            suppress_guild_reminder_notifications: false,
            suppress_join_notification_replies: false,
            suppress_role_subscription_purchase_notifications: false,
            suppress_role_subscription_purchase_notification_replies: false

  @typedoc "Suppress member join notifications"
  @type suppress_join_notifications :: boolean

  @typedoc "Suppress server boost notifications"
  @type suppress_premium_subscriptions :: boolean

  @typedoc "Suppress server setup tips"
  @type suppress_guild_reminder_notifications :: boolean

  @typedoc "Hide member join sticker reply buttons"
  @type suppress_join_notification_replies :: boolean

  @typedoc "Suppress role subscription purchase and renewal notifications"
  @type suppress_role_subscription_purchase_notifications :: boolean

  @typedoc "Hide role subscription sticker reply buttons"
  @type suppress_role_subscription_purchase_notifications_replies :: boolean

  @type flags :: %__MODULE__{
          suppress_join_notifications: suppress_join_notifications,
          suppress_premium_subscriptions: suppress_premium_subscriptions,
          suppress_guild_reminder_notifications: suppress_guild_reminder_notifications,
          suppress_join_notification_replies: suppress_join_notification_replies,
          suppress_role_subscription_purchase_notifications:
            suppress_role_subscription_purchase_notifications,
          suppress_role_subscription_purchase_notification_replies:
            suppress_role_subscription_purchase_notifications_replies
        }

  @type t :: flags

  @flag_values [
    suppress_join_notifications: 1 <<< 0,
    suppress_premium_subscriptions: 1 <<< 1,
    suppress_guild_reminder_notifications: 1 <<< 2,
    suppress_join_notification_replies: 1 <<< 3,
    suppress_role_subscription_purchase_notifications: 1 <<< 4,
    suppress_role_subscription_purchase_notification_replies: 1 <<< 5
  ]

  @doc """
  Constructs a flag struct based on an integer from the Discord API, normally from `t:Nostrum.Struct.Guild.Member.flags/0`.

  ## Examples

  ```elixir
  iex> Nostrum.Struct.Guild.SystemChannelFlags.from_integer(3)
  %Nostrum.Struct.Guild.SystemChannelFlags{
    suppress_guild_reminder_notifications: false,
    suppress_join_notification_replies: false,
    suppress_join_notifications: true,
    suppress_premium_subscriptions: true,
    suppress_role_subscription_purchase_notification_replies: false,
    suppress_role_subscription_purchase_notifications: false
  }
  ```
  """
  @spec from_integer(integer()) :: t
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
  iex> my_flags = %Nostrum.Struct.Guild.SystemChannelFlags{
  ...>               suppress_join_notifications: true,
  ...>               suppress_join_notification_replies: true
  ...> }
  iex> Nostrum.Struct.Guild.SystemChannelFlags.to_integer(my_flags)
  9
  ```
  """
  @spec to_integer(t) :: integer()
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
