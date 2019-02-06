defmodule Nostrum.Struct.Guild.AuditLog do
  @moduledoc """
  Represents a guild's audit log.
  """

  alias Nostrum.Struct.Guild.AuditLogEntry
  alias Nostrum.Struct.{User, Webhook}
  alias Nostrum.Util

  defstruct [:entries, :users, :webhooks]

  @typedoc "Entries of this guild's audit log."
  @type entries :: [AuditLogEntry.t()]

  @typedoc "Users found in the audit log."
  @type users :: [User.t()]

  @typedoc "Webhooks found in the audit log."
  @type webhooks :: [Webhook.t()]

  @type t :: %__MODULE__{
          entries: entries,
          users: users,
          webhooks: webhooks
        }

  @doc false
  def to_struct(map) do
    struct(
      __MODULE__,
      %{
        entries: Util.cast(map.audit_log_entries, {:list, {:struct, AuditLogEntry}}),
        users: Util.cast(map.users, {:list, {:struct, User}}),
        webhooks: Util.cast(map.webhooks, {:list, {:struct, Webhook}})
      }
    )
  end
end
