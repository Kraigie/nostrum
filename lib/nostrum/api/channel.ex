defmodule Nostrum.Api.Channel do
  @moduledoc """
  Functions for interacting with the Discord API's channel endpoints.

  See: https://discord.com/developers/docs/resources/channel
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Snowflake
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.AuditLogEntry
  alias Nostrum.Struct.Message
  alias Nostrum.Struct.Webhook

  import Nostrum.Snowflake, only: [is_snowflake: 1]

  @doc ~S"""
  Pins a message in a channel.

  This endpoint requires the 'VIEW_CHANNEL', 'READ_MESSAGE_HISTORY', and
  'MANAGE_MESSAGES' permissions. It fires the
  `t:Nostrum.Consumer.message_update/0` and
  `t:Nostrum.Consumer.channel_pins_update/0` events.

  If successful, returns `{:ok}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Channel.pin_message(43189401384091, 18743893102394)
  ```
  """
  @spec pin_message(Channel.id(), Message.id()) :: Api.error() | {:ok}
  def pin_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Api.request(:put, Constants.channel_pin(channel_id, message_id))
  end

  @doc """
  Deletes multiple messages from a channel.

  `messages` is a list of `Nostrum.Struct.Message.id` that you wish to delete.
  When given more than 100 messages, this function will chunk the given message
  list into blocks of 100 and send them off to the API. It will stop deleting
  on the first error that occurs. Keep in mind that deleting thousands of
  messages will take a pretty long time and it may be proper to just delete
  the channel you want to bulk delete in and recreate it.

  This method can only delete messages sent within the last two weeks.
  `Filter` is an optional parameter that specifies whether messages sent over
  two weeks ago should be filtered out; defaults to `true`.
  """
  @spec bulk_delete_messages(Channel.id(), [Message.id()], boolean) :: Api.error() | {:ok}
  def bulk_delete_messages(channel_id, messages, filter \\ true)

  def bulk_delete_messages(channel_id, messages, false),
    do: send_chunked_delete(messages, channel_id)

  def bulk_delete_messages(channel_id, messages, true) do
    snowflake_two_weeks_ago =
      DateTime.utc_now()
      |> DateTime.to_unix()
      # 60 seconds * 60 * 24 * 14 = 14 days / 2 weeks
      |> Kernel.-(60 * 60 * 24 * 14)
      |> DateTime.from_unix!()
      |> Snowflake.from_datetime!()

    messages
    |> Stream.filter(&(&1 > snowflake_two_weeks_ago))
    |> send_chunked_delete(channel_id)
  end

  @spec send_chunked_delete(
          [Message.id()] | Enum.t(),
          Snowflake.t()
        ) :: Api.error() | {:ok}
  defp send_chunked_delete(messages, channel_id) do
    messages
    |> Stream.chunk_every(100)
    |> Stream.map(fn message_chunk ->
      Api.request(
        :post,
        Constants.channel_bulk_delete(channel_id),
        %{messages: message_chunk}
      )
    end)
    |> Enum.find({:ok}, &match?({:error, _}, &1))
  end

  @doc """
  Creates a channel for a guild.

  This endpoint requires the `MANAGE_CHANNELS` permission. It fires a
  `t:Nostrum.Consumer.channel_create/0` event.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - channel name (2-100 characters)
    * `:type` (integer) - the type of channel (See `Nostrum.Struct.Channel`)
    * `:topic` (string) - channel topic (0-1024 characters)
    * `:bitrate` (integer) - the bitrate (in bits) of the voice channel (voice only)
    * `:user_limit` (integer) - the user limit of the voice channel (voice only)
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0` or equivalent map) -
    the channel's permission overwrites
    * `:parent_id` (`t:Nostrum.Struct.Channel.id/0`) - id of the parent category for a channel
    * `:nsfw` (boolean) - if the channel is nsfw

  `:name` is always required.

  ## Examples

  ```elixir
  Nostrum.Api.Channel.create(81384788765712384, name: "elixir-nostrum", topic: "craig's domain")
  {:ok, %Nostrum.Struct.Channel{guild_id: 81384788765712384}}
  ```
  """
  @spec create(Guild.id(), Api.options()) :: Api.error() | {:ok, Channel.guild_channel()}
  def create(guild_id, options)

  def create(guild_id, options) when is_list(options),
    do: create(guild_id, Map.new(options))

  def create(guild_id, %{} = options) when is_snowflake(guild_id) do
    Api.request(:post, Constants.guild_channels(guild_id), options)
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @doc ~S"""
  Deletes a channel.

  An optional `reason` can be provided for the guild audit log.

  If deleting a `t:Nostrum.Struct.Channel.guild_channel/0`, this endpoint requires
  the `MANAGE_CHANNELS` permission. It fires a
  `t:Nostrum.Consumer.channel_delete/0`. If a `t:Nostrum.Struct.Channel.guild_category_channel/0`
  is deleted, then a `t:Nostrum.Consumer.channel_update/0` event will fire
  for each channel under the category.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Channel.delete(421533712753360896)
  {:ok, %Nostrum.Struct.Channel{id: 421533712753360896}}
  ```
  """
  @spec delete(Channel.id(), AuditLogEntry.reason()) :: Api.error() | {:ok, Channel.t()}
  def delete(channel_id, reason \\ nil) when is_snowflake(channel_id) do
    %{
      method: :delete,
      route: Constants.channel(channel_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Delete a channel permission for a user or role.

  Role or user overwrite to delete is specified by `channel_id` and `overwrite_id`.
  An optional `reason` can be given for the audit log.
  """
  @spec delete_permissions(Channel.id(), integer, AuditLogEntry.reason()) :: Api.error() | {:ok}
  def delete_permissions(channel_id, overwrite_id, reason \\ nil) do
    Api.request(%{
      method: :delete,
      route: Constants.channel_permission(channel_id, overwrite_id),
      body: "",
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc """
  Unpins a message in a channel.

  This endpoint requires the 'VIEW_CHANNEL', 'READ_MESSAGE_HISTORY', and
  'MANAGE_MESSAGES' permissions. It fires the
  `t:Nostrum.Consumer.message_update/0` and
  `t:Nostrum.Consumer.channel_pins_update/0` events.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec unpin_message(Channel.id(), Message.id()) :: Api.error() | {:ok}
  def unpin_message(channel_id, message_id)
      when is_snowflake(channel_id) and is_snowflake(message_id) do
    Api.request(:delete, Constants.channel_pin(channel_id, message_id))
  end

  @doc """
  Edit the permission overwrites for a user or role.

  Role or user to overwrite is specified by `overwrite_id`.

  `permission_info` is a map with the following keys:
   * `type` - Required; `member` if editing a user, `role` if editing a role.
   * `allow` - Bitwise value of allowed permissions.
   * `deny` - Bitwise value of denied permissions.
   * `type` - `member` if editing a user, `role` if editing a role.

  An optional `reason` can be provided for the audit log.

   `allow` and `deny` are defaulted to `0`, meaning that even if you don't
   specify them, they will override their respective former values in an
   existing overwrite.
  """
  @spec edit_permissions(
          Channel.id(),
          integer,
          %{
            required(:type) => String.t(),
            optional(:allow) => integer,
            optional(:deny) => integer
          },
          AuditLogEntry.reason()
        ) :: Api.error() | {:ok}
  def edit_permissions(channel_id, overwrite_id, permission_info, reason \\ nil) do
    Api.request(%{
      method: :put,
      route: Constants.channel_permission(channel_id, overwrite_id),
      body: permission_info,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
  end

  @doc ~S"""
  Gets a channel.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Channel.get(381889573426429952)
  {:ok, %Nostrum.Struct.Channel{id: 381889573426429952}}
  ```
  """
  @spec get(Channel.id()) :: Api.error() | {:ok, Channel.t()}
  def get(channel_id) when is_snowflake(channel_id) do
    Api.request(:get, Constants.channel(channel_id))
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @doc ~S"""
  Retrieves a channel's messages around a `locator` up to a `limit`.

  This endpoint requires the 'VIEW_CHANNEL' permission. If the current user
  is missing the 'READ_MESSAGE_HISTORY' permission, then this function will
  return no messages.

  If successful, returns `{:ok, messages}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Channel.messages(43189401384091, 5, {:before, 130230401384})
  ```
  """
  @spec messages(Channel.id(), Api.limit(), Api.locator()) ::
          Api.error() | {:ok, [Message.t()]}
  def messages(channel_id, limit, locator \\ {}) when is_snowflake(channel_id) do
    messages_sync(channel_id, limit, [], locator)
  end

  defp messages_sync(channel_id, limit, messages, locator) when limit <= 100 do
    case messages_call(channel_id, limit, locator) do
      {:ok, new_messages} -> {:ok, messages ++ new_messages}
      other -> other
    end
  end

  defp messages_sync(channel_id, limit, messages, locator) do
    case messages_call(channel_id, 100, locator) do
      {:error, message} ->
        {:error, message}

      {:ok, []} ->
        {:ok, messages}

      {:ok, new_messages} ->
        new_limit = get_new_limit(limit, length(new_messages))
        new_locator = get_new_locator(locator, List.last(new_messages))
        messages_sync(channel_id, new_limit, messages ++ new_messages, new_locator)
    end
  end

  defp get_new_locator({}, last_message), do: {:before, last_message.id}
  defp get_new_locator(locator, last_message), do: put_elem(locator, 1, last_message.id)

  defp get_new_limit(:infinity, _new_message_count), do: :infinity
  defp get_new_limit(limit, message_count), do: limit - message_count

  # We're decoding the response at each call to catch any errors
  @doc false
  def messages_call(channel_id, limit, locator) do
    qs_params =
      case locator do
        {} -> [{:limit, limit}]
        non_empty_locator -> [{:limit, limit}, non_empty_locator]
      end

    Api.request(:get, Constants.channel_messages(channel_id), "", qs_params)
    |> Helpers.handle_request_with_decode({:list, {:struct, Message}})
  end

  @doc """
  Gets a list of webhooks for a channel.

  ## Parameters
    - `channel_id` - Channel to get webhooks for.
  """
  @spec webhooks(Channel.id()) :: Api.error() | {:ok, [Webhook.t()]}
  def webhooks(channel_id) do
    Api.request(:get, Constants.webhooks_channel(channel_id))
    |> Helpers.handle_request_with_decode()
  end

  @doc ~S"""
  Retrieves all pinned messages from a channel.

  This endpoint requires the 'VIEW_CHANNEL' and 'READ_MESSAGE_HISTORY' permissions.

  If successful, returns `{:ok, messages}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Channel.pinned_messages(43189401384091)
  ```
  """
  @spec pinned_messages(Channel.id()) :: Api.error() | {:ok, [Message.t()]}
  def pinned_messages(channel_id) when is_snowflake(channel_id) do
    Api.request(:get, Constants.channel_pins(channel_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, Message}})
  end

  @doc ~S"""
  Modifies a channel's settings.

  An optional `reason` can be given for the guild audit log.

  If a `t:Nostrum.Struct.Channel.guild_channel/0` is being modified, this
  endpoint requires the `MANAGE_CHANNEL` permission. It fires a
  `t:Nostrum.Consumer.channel_update/0` event. If a
  `t:Nostrum.Struct.Channel.guild_category_channel/0` is being modified, then this
  endpoint fires multiple `t:Nostrum.Consumer.channel_update/0` events.

  If successful, returns `{:ok, channel}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:name` (string) - 2-100 character channel name
    * `:position` (integer) - the position of the channel in the left-hand listing
    * `:topic` (string) (`t:Nostrum.Struct.Channel.text_channel/0` only) -
    0-1024 character channel topic
    * `:nsfw` (boolean) (`t:Nostrum.Struct.Channel.text_channel/0` only) -
    if the channel is nsfw
    * `:bitrate` (integer) (`t:Nostrum.Struct.Channel.voice_channel/0` only) -
    the bitrate (in bits) of the voice channel; 8000 to 96000 (128000 for VIP servers)
    * `:user_limit` (integer) (`t:Nostrum.Struct.Channel.voice_channel/0` only) -
    the user limit of the voice channel; 0 refers to no limit, 1 to 99 refers to a user limit
    * `:permission_overwrites` (list of `t:Nostrum.Struct.Overwrite.t/0` or equivalent map) -
    channel or category-specific permissions
    * `:parent_id` (`t:Nostrum.Struct.Channel.id/0`) (`t:Nostrum.Struct.Channel.guild_channel/0` only) -
    id of the new parent category for a channel

  ## Examples

  ```elixir
  Nostrum.Api.Channel.modify(41771983423143933, name: "elixir-nostrum", topic: "nostrum discussion")
  {:ok, %Nostrum.Struct.Channel{id: 41771983423143933, name: "elixir-nostrum", topic: "nostrum discussion"}}

  Nostrum.Api.Channel.modify(41771983423143933)
  {:ok, %Nostrum.Struct.Channel{id: 41771983423143933}}
  ```
  """
  @spec modify(Channel.id(), Api.options(), AuditLogEntry.reason()) ::
          Api.error() | {:ok, Channel.t()}
  def modify(channel_id, options, reason \\ nil)

  def modify(channel_id, options, reason) when is_list(options),
    do: modify(channel_id, Map.new(options), reason)

  def modify(channel_id, %{} = options, reason) when is_snowflake(channel_id) do
    %{
      method: :patch,
      route: Constants.channel(channel_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Triggers the typing indicator.

  Triggers the typing indicator in the channel specified by `channel_id`.
  The typing indicator lasts for about 8 seconds and then automatically stops.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec start_typing(integer) :: Api.error() | {:ok}
  def start_typing(channel_id) do
    Api.request(:post, Constants.channel_typing(channel_id))
  end
end
