defmodule Nostrum.Api.Thread do
  @moduledoc """
  Functions for interacting with the Discord API's thread endpoints.

  Endpoints related to threads in the Discord Channels API: https://discord.com/developers/docs/resources/channel#start-thread-from-message
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.AuditLogEntry
  alias Nostrum.Struct.Message
  alias Nostrum.Struct.ThreadMember
  alias Nostrum.Struct.User
  alias Nostrum.Util

  import Api.Helpers, only: [has_files: 1]

  @doc """
  Add a user to a thread, requires the ability to send messages in the thread.
  """
  @doc since: "1.x.x"
  @spec add_member(Channel.id(), User.id()) :: {:ok} | Api.error()
  def add_member(thread_id, user_id) do
    Api.request(:put, Constants.thread_member(thread_id, user_id))
  end

  @doc """
  Returns a thread member object for the specified user if they are a member of the thread
  """
  @doc since: "1.x.x"
  @spec member(Channel.id(), User.id()) :: {:ok, ThreadMember.t()} | Api.error()
  def member(thread_id, user_id) do
    Api.request(:get, Constants.thread_member(thread_id, user_id))
    |> Helpers.handle_request_with_decode({:struct, ThreadMember})
  end

  @doc """
  Returns a list of thread members for the specified thread.

  This endpoint is restricted according to whether the `GUILD_MEMBERS` privileged intent is enabled.
  """
  @doc since: "1.x.x"
  @spec members(Channel.id()) :: {:ok, [ThreadMember.t()]} | Api.error()
  def members(thread_id) do
    Api.request(:get, Constants.thread_members(thread_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, ThreadMember}})
  end

  @doc """
  Join an existing thread, requires that the thread is not archived.
  """
  @doc since: "1.x.x"
  @spec join(Channel.id()) :: {:ok} | Api.error()
  def join(thread_id) do
    Api.request(:put, Constants.thread_member_me(thread_id))
  end

  @doc """
  Leave a thread, requires that the thread is not archived.
  """
  @doc since: "1.x.x"
  @spec leave(Channel.id()) :: {:ok} | Api.error()
  def leave(thread_id) do
    Api.request(:delete, Constants.thread_member_me(thread_id))
  end

  @doc """
  Return all active threads for the current guild.

  Response body is a map with the following keys:
  - `threads`: A list of channel objects.
  - `members`: A list of `ThreadMember` objects, one for each returned thread the current user has joined.
  """
  @doc since: "1.x.x"
  @spec list(Guild.id()) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()]}} | Api.error()
  def list(guild_id) do
    res =
      Api.request(:get, Constants.guild_active_threads(guild_id))
      |> Helpers.handle_request_with_decode()

    case res do
      {:ok, %{threads: channels, members: thread_members}} ->
        map = %{
          threads: Util.cast(channels, {:list, {:struct, Channel}}),
          members: Util.cast(thread_members, {:list, {:struct, ThreadMember}})
        }

        {:ok, map}

      {:error, e} ->
        {:error, e}
    end
  end

  @doc """
  Same as `public_archived_threads/2`, but only returns private threads that the current user has joined.
  """
  @doc since: "1.x.x"
  @spec joined_private_archived_threads(Channel.id(), Api.options()) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()], has_more: boolean()}}
          | Api.error()
  def joined_private_archived_threads(channel_id, options \\ [])

  def joined_private_archived_threads(channel_id, options) when is_map(options) do
    Constants.private_joined_archived_threads(channel_id)
    |> list_archived_threads(Map.to_list(options))
  end

  def joined_private_archived_threads(channel_id, options) when is_list(options) do
    Constants.private_joined_archived_threads(channel_id)
    |> list_archived_threads(options)
  end

  defp list_archived_threads(route, options) do
    options = options |> Helpers.maybe_convert_date_time(:before)

    res =
      Api.request(%{
        method: :get,
        route: route,
        body: "",
        params: options,
        headers: []
      })
      |> Helpers.handle_request_with_decode()

    case res do
      {:ok, %{threads: channels, members: thread_members, has_more: has_more}} ->
        map = %{
          threads: Util.cast(channels, {:list, {:struct, Channel}}),
          members: Util.cast(thread_members, {:list, {:struct, ThreadMember}}),
          has_more: has_more
        }

        {:ok, map}

      {:error, e} ->
        {:error, e}
    end
  end

  @doc """
  Same as `public_archived_threads/2`, but for private threads instead of public.
  """
  @doc since: "1.x.x"
  @spec private_archived_threads(Channel.id(), Api.options()) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()], has_more: boolean()}}
          | Api.error()
  def private_archived_threads(channel_id, options \\ [])

  def private_archived_threads(channel_id, options) when is_map(options) do
    Constants.private_archived_threads(channel_id)
    |> list_archived_threads(Map.to_list(options))
  end

  def private_archived_threads(channel_id, options) when is_list(options) do
    Constants.private_archived_threads(channel_id)
    |> list_archived_threads(options)
  end

  @doc """
  Returns a list of archived threads for a given channel.

  Threads are sorted by the `archive_timestamp` field, in descending order.

  ## Response body
  Response body is a map with the following keys:
  - `threads`: A list of channel objects.
  - `members`: A list of `ThreadMember` objects, one for each returned thread the current user has joined.
  - `has_more`: A boolean indicating whether there are more archived threads that can be fetched.

  ## Options
  - `before`: Returns threads before this timestamp, can be either a `DateTime` or [ISO8601 timestamp](`DateTime.to_iso8601/3`).
  - `limit`: Optional maximum number of threads to return.
  """
  @doc since: "1.x.x"
  @spec public_archived_threads(Channel.id(), Api.options()) ::
          {:ok, %{threads: [Channel.t()], members: [ThreadMember.t()], has_more: boolean()}}
          | Api.error()
  def public_archived_threads(channel_id, options \\ [])

  def public_archived_threads(channel_id, options) when is_map(options) do
    Constants.public_archived_threads(channel_id)
    |> list_archived_threads(Map.to_list(options))
  end

  def public_archived_threads(channel_id, options) when is_list(options) do
    Constants.public_archived_threads(channel_id)
    |> list_archived_threads(options)
  end

  @doc """
  Removes another user from a thread, requires that the thread is not archived.

  Also requires the `MANAGE_THREADS` permission, or the creator of the thread if the thread is private.
  """
  @doc since: "1.x.x"
  @spec remove_member(Channel.id(), User.id()) :: {:ok} | Api.error()
  def remove_member(thread_id, user_id) do
    Api.request(:delete, Constants.thread_member(thread_id, user_id))
  end

  @type thread_without_message_params :: %{
          required(:name) => String.t(),
          required(:type) => non_neg_integer(),
          optional(:auto_archive_duration) => 60 | 1440 | 4320 | 10_080,
          optional(:invitable) => boolean(),
          optional(:rate_limit_per_user) => 0..21_600
        }

  @doc """
  Create a thread on a channel without an associated message.

  If successful, returns `{:ok, Channel}`. Otherwise returns a `t:Nostrum.Api.error/0`.

  An optional `reason` argument can be given for the audit log.

  ## Options
  - `name`: Name of the thread, max 100 characters.
  - `type`: Type of thread, can be either 11 (`GUILD_PUBLIC_THREAD`) or 12 (`GUILD_PRIVATE_THREAD`).
  - `auto_archive_duration`: Duration in minutes to auto-archive the thread after it has been inactive, can be set to 60, 1440, 4320, or 10080.
  - `invitable`: whether non-moderators can add other non-moderators to a thread; only available when creating a private thread defaults to `false`.
  - `rate_limit_per_user`: Rate limit per user in seconds, can be set to any value in `0..21600`.
  """
  @doc since: "1.x.x"
  @spec create(Channel.id(), thread_without_message_params, AuditLogEntry.reason()) ::
          {:ok, Channel.t()} | Api.error()
  def create(channel_id, options, reason \\ nil) do
    Api.request(%{
      method: :post,
      route: Constants.thread_without_message(channel_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @doc """
  Create a new thread in a forum channel.

  If successful, returns `{:ok, Channel}`. Otherwise returns a `t:Nostrum.Api.error/0`.

  An optional `reason` argument can be given for the audit log.

  ## Options
  - `name`: Name of the thread, max 100 characters.
  - `auto_archive_duration`: Duration in minutes to auto-archive the thread after it has been inactive, can be set to 60, 1440, 4320, or 10080.
  - `rate_limit_per_user`: Rate limit per user in seconds, can be set to any value in `0..21600`.
  - `applied_tags`: An array of tag ids to apply to the thread.
  - `message`: The first message in the created thread.

  ### Thread Message Options
  - `content`: The content of the message.
  - `embeds`: A list of embeds.
  - `allowed_mentions`: Allowed mentions object.
  - `components`: A list of components.
  - `sticker_ids`: A list of sticker ids.
  - `:files` - a list of files where each element is the same format as the `:file` option. If both
    `:file` and `:files` are specified, `:file` will be prepended to the `:files` list.

  At least one of `content`, `embeds`, `sticker_ids`, or `files` must be specified.
  """
  @doc since: "1.x.x"
  @spec create_in_forum(Channel.id(), map(), AuditLogEntry.reason()) ::
          {:ok, Channel.t()} | Api.error()
  def create_in_forum(channel_id, options, reason \\ nil)

  def create_in_forum(channel_id, %{message: data} = body, reason)
      when has_files(data) do
    # done this way to avoid breaking changes to support audit log reasons in multipart requests
    boundary = Helpers.generate_boundary()

    {files, json} =
      Helpers.combine_files(body)
      |> Helpers.pop_files()

    body = Jason.encode_to_iodata!(json)

    headers =
      Helpers.maybe_add_reason(reason, [
        {"content-type", "multipart/form-data; boundary=#{boundary}"}
      ])

    %{
      method: :post,
      route: Constants.thread_without_message(channel_id),
      body: {:multipart, Api.create_multipart(files, body, boundary)},
      params: [],
      headers: headers
    }
    |> Api.request()
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  def create_in_forum(channel_id, options, reason) do
    Api.request(%{
      method: :post,
      route: Constants.thread_without_message(channel_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end

  @type thread_with_message_params :: %{
          required(:name) => String.t(),
          optional(:auto_archive_duration) => 60 | 1440 | 4320 | 10_080,
          optional(:rate_limit_per_user) => 0..21_600
        }

  @doc """
  Create a thread on a channel message.

  The `thread_id` will be the same as the id of the message, as such no message can have more than one thread.

  If successful, returns `{:ok, Channel}`. Otherwise returns a `t:Nostrum.Api.error/0`.

  An optional `reason` argument can be given for the audit log.

  ## Options
  - `name`: Name of the thread, max 100 characters.
  - `auto_archive_duration`: Duration in minutes to auto-archive the thread after it has been inactive, can be set to 60, 1440, 4320, or 10080.
  - `rate_limit_per_user`: Rate limit per user in seconds, can be set to any value in `0..21600`.

  """
  @doc since: "1.x.x"
  @spec create_with_message(
          Channel.id(),
          Message.id(),
          thread_with_message_params,
          AuditLogEntry.reason()
        ) ::
          {:ok, Channel.t()} | Api.error()
  def create_with_message(channel_id, message_id, options, reason \\ nil) do
    Api.request(%{
      method: :post,
      route: Constants.thread_with_message(channel_id, message_id),
      body: options,
      params: [],
      headers: Helpers.maybe_add_reason(reason)
    })
    |> Helpers.handle_request_with_decode({:struct, Channel})
  end
end
