defmodule Nostrum.Struct.Event.AutoModerationRuleExecute do
  @moduledoc """
  Sent when an auto-moderation rule executes.
  (e.g. message is blocked).
  """
  @moduledoc since: "0.7.0"

  alias Nostrum.Struct.{AutoModerationRule, Channel, Guild, Message, User}
  alias Nostrum.{Snowflake, Util}

  defstruct [
    :guild_id,
    :action,
    :rule_id,
    :rule_trigger_type,
    :user_id,
    :channel_id,
    :message_id,
    :alert_system_message_id,
    :content,
    :matched_keyword,
    :matched_content
  ]

  @typedoc "The id of the guild in which the action was executed"
  @type guild_id :: Guild.id()

  @typedoc "The action that was executed"
  @type action :: AutoModerationRule.Action.t()

  @typedoc "The id of the rule that was executed"
  @type rule_id :: AutoModerationRule.id()

  @typedoc "The type of the rule that was executed"
  @type rule_trigger_type :: AutoModerationRule.trigger_type()

  @typedoc "The id of the user which generated the content which triggered the rule"
  @type user_id :: User.id()

  @typedoc """
  The id of the channel in which the content was posted

  note: this field may not exist if the content was blocked from being created
  """
  @type channel_id :: Channel.id() | nil

  @typedoc """
  The id of the message which was posted

  note: this field will not exist if the content was blocked from being created
  """
  @type message_id :: Message.id() | nil

  @typedoc """
  The id of any system message that was generated as a result of the action

  note: will not exist if the event does not correspond to an action that generates a system message
  """
  @type alert_system_message_id :: Message.id() | nil

  @typedoc "The content of the message which triggered the rule"
  @type content :: String.t()

  @typedoc """
  The keyword that was matched in the content
  """
  @type matched_keyword :: String.t() | nil

  @typedoc """
  The substring which matched the content
  """
  @type matched_content :: String.t() | nil

  @type t :: %__MODULE__{
          guild_id: guild_id(),
          action: action(),
          rule_id: rule_id(),
          rule_trigger_type: rule_trigger_type(),
          user_id: user_id(),
          channel_id: channel_id(),
          message_id: message_id(),
          alert_system_message_id: alert_system_message_id(),
          content: content(),
          matched_keyword: matched_keyword(),
          matched_content: matched_content()
        }

  @doc false
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:guild_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:action, nil, &Util.cast(&1, {:struct, AutoModerationRule.Action}))
      |> Map.update(:rule_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:user_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:channel_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:message_id, nil, &Util.cast(&1, Snowflake))
      |> Map.update(:alert_system_message_id, nil, &Util.cast(&1, Snowflake))

    struct(__MODULE__, new)
  end
end
