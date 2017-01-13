defmodule Mixcord.Api do
  @moduledoc """
  Interface for Discord's rest API.
  """

  # TODO: Upload file

  alias Mixcord.Constants
  alias Mixcord.Shard

  @typedoc """
  Represents a failed response from the API.

  This occurs when hackney or HTTPoison fail, or when the API doesn't respond with `200` or `204`.
  """
  @type error :: {:error, Mixcord.Error.ApiError.t}

  @typedoc """
  Represents a tuple used to locate messages.

  The first element of the tuple is an atom.
  The second element will be a message_id as an integer.
  The tuple can also be empty to search from the most recent message in the channel
  """
  @type locator :: {:before, Integer.t} | {:after, Integer.t} | {:around, Integer.t} | {}

  @typedoc """
  Represents a limit used to retrieve messages.

  Integer number of messages, or :infinity to retrieve all messages.
  """
  @type limit :: Integer.t | :infinity

  def update_status(pid, _status, game) when not is_map(game) and not is_pid(pid), do: raise "ERROR: Invalid game map or shard pid #{inspect game} #{inspect pid}"
  def update_status(pid, status, game) do
    Shard.update_status(pid, status, game)
  end

  def update_status(_status, game) when not is_map(game), do: raise "ERROR: Invalid game map #{inspect game}"
  def update_status(status, game) when is_map(game) do
    Shard.Supervisor.update_status(status, game)
  end

  @doc """
  Send a message to a channel.

  Send `content` to the channel identified with `channel_id`.
  Content can be either a binary containing the message you want to send or a `Mixcord.Map.Embed` map.

  `tts` is an optional parameter that dictates whether the message should be played over text to speech.

  Returns `{:ok, Mixcord.Map.Message}` if successful. `error` otherwise.
  """
  @spec create_message(Integer.t, String.t, boolean) :: error | {:ok, Mixcord.Map.Message.t}
  def create_message(channel_id, content, tts \\ false)
  def create_message(channel_id, content, tts) when is_binary(content) do
    case request(:post, Constants.channel_messages(channel_id), %{content: content, tts: tts}) do
      {:ok, body} ->
        {:ok, Poison.decode!(body, as: %Mixcord.Map.Message{})}
      other ->
        other
    end
  end

  @spec create_message(Integer.t, Map.t, boolean) :: error | {:ok, Mixcord.Map.Message.t}
  def create_message(channel_id, content, tts) when is_map(content) do
    case request(:post, Constants.channel_messages(channel_id), %{embed: content, tts: tts}) do
      {:ok, body} ->
        {:ok, Poison.decode!(body, as: %Mixcord.Map.Message{})}
      other ->
        other
    end
  end

  @doc """
  Send a message to a channel.

  Send `content` to the channel identified with `channel_id`.
  `tts` is an optional parameter that dictates whether the message should be played over text to speech.

  Raises `Mixcord.Error.ApiError` if error occurs while making the rest call.
  Returns `Mixcord.Map.Message` if successful.
  """
  @spec create_message!(Integer.t, String.t, boolean) :: no_return | Mixcord.Map.Message.t
  def create_message!(channel_id, content, tts \\ false) do
    create_message(channel_id, content, tts)
    |> bangify
  end

  @doc """
  Edit a message.

  Edit a message with the given `content`. Message to edit is specified by `channel_id` and `message_id`.

  Returns the edited `{:ok, Mixcord.Map.Message}` if successful. `error` otherwise.
  """
  @spec edit_message(Integer.t, Integer.t, String.t) :: error | {:ok, Mixcord.Map.Message.t}
  def edit_message(channel_id, message_id, content) do
    case request(:patch, Constants.channel_message(channel_id, message_id), %{content: content}) do
      {:ok, body} ->
        {:ok, Poison.decode!(body, as: %Mixcord.Map.Message{})}
      other ->
        other
    end
  end

  @doc """
  Edit a message.

  Edit a message with the given `content`. Message to edit is specified by `channel_id` and `message_id`.

  Raises `Mixcord.Error.ApiError` if error occurs while making the rest call.
  Returns the edited `Mixcord.Map.Message` if successful.
  """
  @spec edit_message!(Integer.t, Integer.t, String.t) :: no_return | {:ok, Mixcord.Map.Message.t}
  def edit_message!(channel_id, message_id, content) do
    edit_message(channel_id, message_id, content)
    |> bangify
  end

  @doc """
  Delete a message.

  Delete a message specified by `channel_id` and `message_id`.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  @spec delete_message(Integer.t, Integer.t) :: error | {:ok}
  def delete_message(channel_id, message_id) do
    request(:delete, Constants.channel_message(channel_id, message_id))
  end

  @doc """
  Delete a message.

  Delete a message specified by `channel_id` and `message_id`.

  Raises `Mixcord.Error.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  @spec delete_message!(String.t, Integer.t) :: no_return | {:ok}
  def delete_message!(channel_id, message_id) do
    delete_message(channel_id, message_id)
    |> bangify
  end

  def get_channel(channel_id) do
    request(:get, Constants.channel(channel_id))
  end

  def get_channel!(channel_id) do
    get_channel(channel_id)
    |> bangify
  end

  def edit_channel(channel_id, options) do
    request(:patch, Constants.channel(channel_id), options)
  end

  def edit_channel!(channel_id, options) do
    edit_channel(channel_id, options)
    |> bangify
  end

  def delete_channel(channel_id) do
    request(:delete, Constants.channel(channel_id))
  end

  def delete_channel!(channel_id) do
    delete_channel(channel_id)
    |> bangify
  end

  @doc """
  Retrieve messages from a channel.

  Retrieves `limit` number of messages from the channel with id `channel_id`.
  `locator` is a tuple indicating what messages you want to retrieve.

  Returns `{:ok, [Mixcord.Map.Message]}` if successful. `error` otherwise.
  """
  @spec get_channel_messages(Integer.t, limit, locator) :: error | {:ok, [Mixcord.Map.Message.t]}
  def get_channel_messages(channel_id, limit, locator) do
    get_messages_sync(channel_id, limit, [], locator)
  end

  defp get_messages_sync(channel_id, limit, messages, locator) when limit <= 100 do
    case get_channel_messages_call(channel_id, limit, locator) do
      {:ok, new_messages} -> {:ok, messages ++ new_messages}
      other -> other
    end
  end

  defp get_messages_sync(channel_id, limit, messages, locator) do
    case get_channel_messages_call(channel_id, 100, locator) do
      {:error, message} -> {:error, message}
      {:ok, []} -> {:ok, messages}
      {:ok, new_messages} ->
        new_limit = get_new_limit(limit, length(new_messages))
        new_locator = get_new_locator(locator, List.last(new_messages))
        get_messages_sync(channel_id, new_limit, messages ++ new_messages, new_locator)
    end
  end

  defp get_new_locator({}, last_message), do: {:before, last_message.id}
  defp get_new_locator(locator, last_message), do: put_elem(locator, 1, last_message.id)

  defp get_new_limit(:infinity, _new_message_count), do: :infinity
  defp get_new_limit(limit, message_count), do: limit - message_count

  # We're decoding the response at each call to catch any errors
  def get_channel_messages_call(channel_id, limit, locator) do
    qs_params =
      case locator do
        {} -> [{:limit, limit}]
        non_empty_locator -> [{:limit, limit}, non_empty_locator]
      end
    response = request(:get, Constants.channel_messages(channel_id), "", params: qs_params)
    case response do
      {:ok, body} ->
        {:ok, Poison.decode!(body, as: [%Mixcord.Map.Message{}])}
      other ->
        other
    end
  end

  def get_channel_messages!(channel_id, limit, locator) do
    get_channel_messages(channel_id, limit, locator)
    |> bangify
  end

  def get_channel_message(channel_id, message_id) do
    request(:get, Constants.channel_message(channel_id, message_id))
  end

  def get_channel_message!(channel_id, message_id) do
    get_channel_message(channel_id, message_id)
    |> bangify
  end

  def bulk_delete_messages(channel_id) do
    request(:delete, Constants.channel_bulk_delete(channel_id))
  end

  def bulk_delete_messages!(channel_id) do
    bulk_delete_messages(channel_id)
    |> bangify
  end

  def edit_channel_permissions(channel_id, overwrite_id) do
    request(:put, Constants.channel_permission(channel_id, overwrite_id))
  end

  def edit_channel_permissions!(channel_id, overwrite_id) do
    edit_channel_permissions(channel_id, overwrite_id)
    |> bangify
  end

  def delete_channel_permissions(channel_id, overwrite_id) do
    request(:delete, Constants.channel_permission(channel_id, overwrite_id))
  end

  def delete_channel_permissions!(channel_id, overwrite_id) do
    delete_channel_permissions(channel_id, overwrite_id)
    |> bangify
  end

  def get_channel_invites(channel_id) do
    request(:get, Constants.channel_invites(channel_id))
  end

  def get_channel_invites!(channel_id) do
    get_channel_invites(channel_id)
    |> bangify
  end

  def create_channel_invite(channel_id, options \\ %{}) do
    request(:post, Constants.channel_invites(channel_id), options)
  end

  def create_channel_invite!(channel_id, options \\ %{}) do
    create_channel_invite(channel_id, options)
    |> bangify
  end

  @doc """
  Triggers the typing indicator.

  Triggers the typing indicator in the channel specified by `channel_id`.
  The typing indicator lasts for about 8 seconds and then automatically stops.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  def start_typing(channel_id) do
    request(:post, Constants.channel_typing(channel_id))
  end


  @doc """
  Triggers the typing indicator.

  Triggers the typing indicator in the channel specified by `channel_id`.
  The typing indicator lasts for about 8 seconds and then automatically stops.

  Raises `Mixcord.Error.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  def start_typing!(channel_id) do
    start_typing(channel_id)
    |> bangify
  end

  def get_pinned_messages(channel_id) do
    request(:get, Constants.channel_pins(channel_id))
  end

  def get_pinned_messages!(channel_id) do
    get_pinned_messages(channel_id)
    |> bangify
  end

  @doc """
  Pins a message.

  Pins the message specified by `message_id` in the channel specified by `channel_id`.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  def add_pinned_message(channel_id, message_id) do
    request(:put, Constants.channel_pin(channel_id, message_id))
  end

  @doc """
  Pins a message.

  Pins the message specified by `message_id` in the channel specified by `channel_id`.

  Raises `Mixcord.Error.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  def add_pinned_message!(channel_id, message_id) do
    add_pinned_message(channel_id, message_id)
    |> bangify
  end

  @doc """
  Unpins a message.

  Unpins the message specified by `message_id` in the channel specified by `channel_id`.

  Returns `{:ok}` if successful. `error` otherwise.
  """
  def delete_pinned_message(channel_id, message_id) do
    request(:delete, Constants.channel_pin(channel_id, message_id))
  end

  @doc """
  Unpins a message.

  Unpins the message specified by `message_id` in the channel specified by `channel_id`.

  Raises `Mixcord.Error.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  def delete_pinned_message!(channel_id, message_id) do
    delete_pinned_message(channel_id, message_id)
    |> bangify
  end

  def get_guild(guild_id) do
    request(:get, Constants.guild(guild_id))
  end

  def edit_guild(guild_id, options) do
    request(:patch, Constants.guild(guild_id), options)
  end

  def delete_guild(guild_id) do
    request(:delete, Constants.guild(guild_id))
  end

  def create_channel(guild_id, options) do
    request(:post, Constants.guild_channels(guild_id), options)
  end

  def modify_channel_position(guild_id, options) do
    request(:patch, Constants.guild_channels(guild_id), options)
  end

  def get_member(guild_id, user_id) do
    request(:get, Constants.guild_member(guild_id, user_id))
  end

  # TODO: Change or remove option paramter from functions that are not JSON
  def guild_members(guild_id, options) do
    request(:get, Constants.guild_members(guild_id), options)
  end

  def add_member(guild_id, user_id, options) do
    request(:put, Constants.guild_member(guild_id, user_id), options)
  end

  def modify_member(guild_id, user_id, options) do
    request(:patch, Constants.guild_member(guild_id, user_id), options)
  end

  def remove_member(guild_id, user_id) do
    request(:remove, Constants.guild_member(guild_id, user_id))
  end

  def get_guild_bans(guild_id) do
    request(:get, Constants.guild_bans(guild_id))
  end

  def create_guild_ban(guild_id, user_id, options) do
    request(:put, Constants.guild_ban(guild_id, user_id), options)
  end

  def remove_guild_ban(guild_id, user_id) do
    request(:remove, Constants.guild_ban(guild_id, user_id))
  end

  def get_guild_roles(guild_id) do
    request(:get, Constants.guild_roles(guild_id))
  end

  def create_guild_roles(guild_id) do
    request(:post, Constants.guild_roles(guild_id))
  end

  def batch_modify_guild_roles(guild_id, options) do
    request(:patch, Constants.guild_roles(guild_id), options)
  end

  def modify_guild_roles(guild_id, role_id, options) do
    request(:patch, Constants.guild_role(guild_id, role_id), options)
  end

  def delete_guild_role(guild_id, role_id) do
    request(:delete, Constants.guild_role(guild_id, role_id))
  end

  def get_guild_prune(guild_id, options) do
    request(:get, Constants.guild_prune(guild_id), options)
  end

  def begin_guild_prune(guild_id, options) do
    request(:post, Constants.guild_prune(guild_id), options)
  end

  def get_voice_region(guild_id) do
    request(:get, Constants.guild_voice_regions(guild_id))
  end

  def get_guild_invites(guild_id) do
    request(:get, Constants.guild_invites(guild_id))
  end

  def get_guild_integrations(guild_id) do
    request(:get, Constants.guild_integrations(guild_id))
  end

  def create_guild_integrations(guild_id, options) do
    request(:post, Constants.guild_integrations(guild_id), options)
  end

  def modify_guild_integrations(guild_id, integration_id, options) do
    request(:patch, Constants.guild_integration(guild_id, integration_id), options)
  end

  def delete_guild_integrations(guild_id, integration_id) do
    request(:delete, Constants.guild_integration(guild_id, integration_id))
  end

  def sync_guild_integrations(guild_id, integration_id) do
    request(:post, Constants.guild_integration_sync(guild_id, integration_id))
  end

  def get_guild_embed(guild_id) do
    request(:get, Constants.guild_embed(guild_id))
  end

  def modify_guild_embed(guild_id) do
    request(:patch, Constants.guild_embed(guild_id))
  end

  def request(method, route, body \\ "", options \\ []) do
    request = %{
      method: method,
      route: route,
      body: body,
      options: options
    }
    GenServer.call(Ratelimiter, {:queue, request, nil}, :infinity)
  end

  def bangify(to_bang) do
    case to_bang do
      {:error, %{status_code: code, message: message}} ->
        raise(Mixcord.Error.ApiError, status_code: code, message: message)
      {:ok, body} ->
        body
      {:ok} ->
        {:ok}
    end
  end

  @doc """
  Returns the token of the bot.
  """
  @spec get_token() :: String.t
  def get_token() do
    Application.get_env(:mixcord, :token)
  end

end
