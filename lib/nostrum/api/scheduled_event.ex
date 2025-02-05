defmodule Nostrum.Api.ScheduledEvent do
  @moduledoc """
  Module for interacting with the Discord API's scheduled event endpoints.

  See: https://discord.com/developers/docs/resources/guild-scheduled-event
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.AuditLogEntry
  alias Nostrum.Struct.Guild.ScheduledEvent

  @doc """
  Creates a new scheduled event for the guild.

  ## Options
    * `:channel_id` - (`t:Nostrum.Snowflake.t/0`) optional channel id for the event
    * `:entity_metadata` - (`t:Nostrum.Struct.Guild.ScheduledEvent.EntityMetadata.t/0`) metadata for the event
    * `:name` - (string) required name for the event
    * `:privacy_level` - (integer) at the time of writing, this must always be 2 for `GUILD_ONLY`
    * `:scheduled_start_time` - required time for the event to start as a `DateTime` or (ISO8601 timestamp)[`DateTime.to_iso8601/3`]
    * `:scheduled_end_time` - optional time for the event to end as a `DateTime` or (ISO8601 timestamp)[`DateTime.to_iso8601/3`]
    * `:description` - (string) optional description for the event
    * `:entity_type` - (integer) an integer representing the type of entity the event is for
      * `1` - `STAGE_INSTANCE`
      * `2` - `VOICE`
      * `3` - `EXTERNAL`

  See the (official documentation)[https://discord.com/developers/docs/resources/guild-scheduled-event] for more information.


  An optional `reason` can be specified for the audit log.
  """
  @doc since: "1.x.x"
  @spec create(Guild.id(), AuditLogEntry.reason(), Api.options()) ::
          {:ok, ScheduledEvent.t()} | Api.error()
  def create(guild_id, reason \\ nil, options)

  def create(guild_id, reason, options) when is_list(options),
    do: create(guild_id, reason, Map.new(options))

  def create(guild_id, reason, %{} = options) do
    options =
      options
      |> Helpers.maybe_convert_date_time(:scheduled_start_time)
      |> Helpers.maybe_convert_date_time(:scheduled_end_time)

    Api.request(%{
      method: :post,
      route: Constants.guild_scheduled_events(guild_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
    |> Helpers.handle_request_with_decode({:struct, ScheduledEvent})
  end

  @doc """
  Delete a scheduled event for a guild.
  """
  @doc since: "1.x.x"
  @spec delete(Guild.id(), ScheduledEvent.id()) ::
          Api.error() | {:ok}
  def delete(guild_id, event_id) do
    Api.request(:delete, Constants.guild_scheduled_event(guild_id, event_id))
  end

  @doc """
  Get a scheduled event for a guild.
  """
  @doc since: "1.x.x"
  @spec get(Guild.id(), ScheduledEvent.id()) ::
          Api.error() | {:ok, ScheduledEvent.t()}
  def get(guild_id, event_id) do
    Api.request(:get, Constants.guild_scheduled_event(guild_id, event_id))
    |> Helpers.handle_request_with_decode({:struct, ScheduledEvent})
  end

  @doc """
  Get a list of users who have subscribed to an event.

  ## Options
  All are optional, with their default values listed.
  * `:limit` (integer) maximum number of users to return, defaults to `100`
  * `:with_member` (boolean) whether to include the member object for each user, defaults to `false`
  * `:before` (`t:Nostrum.Snowflake.t/0`) return only users before this user id, defaults to `nil`
  * `:after` (`t:Nostrum.Snowflake.t/0`) return only users after this user id, defaults to `nil`
  """
  @doc since: "1.x.x"
  @spec users(Guild.id(), ScheduledEvent.id(), Api.options()) ::
          Api.error() | {:ok, [ScheduledEvent.User.t()]}
  def users(guild_id, event_id, params \\ []) do
    Api.request(:get, Constants.guild_scheduled_event_users(guild_id, event_id), "", params)
    |> Helpers.handle_request_with_decode({:list, {:struct, ScheduledEvent.User}})
  end

  @doc """
  Modify a scheduled event for a guild.

  Options are the same as for `create_guild_scheduled_event/2` except all fields are optional,
  with the additional optional integer field `:status` which can be one of:

    * `1` - `SCHEDULED`
    * `2` - `ACTIVE`
    * `3` - `COMPLETED`
    * `4` - `CANCELLED`

  Copied from the official documentation:
  * If updating entity_type to `EXTERNAL`:
    * `channel_id` is required and must be set to null
    * `entity_metadata` with a `location` field must be provided
    * `scheduled_end_time` must be provided
  """
  @doc since: "1.x.x"
  @spec modify(
          Guild.id(),
          ScheduledEvent.id(),
          AuditLogEntry.reason(),
          Api.options()
        ) :: Api.error() | {:ok, ScheduledEvent.t()}
  def modify(guild_id, event_id, reason \\ nil, options)

  def modify(guild_id, event_id, reason, options) when is_list(options),
    do: modify(guild_id, event_id, reason, Map.new(options))

  def modify(guild_id, event_id, reason, options) when is_map(options) do
    prepared_options =
      options
      |> Helpers.maybe_convert_date_time(:scheduled_start_time)
      |> Helpers.maybe_convert_date_time(:scheduled_end_time)

    Api.request(%{
      method: :patch,
      route: Constants.guild_scheduled_event(guild_id, event_id),
      body: prepared_options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
    |> Helpers.handle_request_with_decode({:struct, ScheduledEvent})
  end
end
