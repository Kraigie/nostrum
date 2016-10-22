defmodule Mixcord.Api do
  @moduledoc """
  Interface for Discord's rest API.
  """

  # TODO: Upload file

  alias Mixcord.Constants
  alias Mixcord.Struct.{Message, User}
  import Mixcord.Api.Ratelimiter

  @doc """
  Send a message to a channel.

  Send `content` to the channel identified with `channel_id`.
  `tts` is an optional parameter that dictates whether the message should be played over text to speech.

  Returns `{:ok, Mixcord.Constructs.Message}` if successful. `{:error, %{status_code: status_code, message: message}}` otherwise.
  """
  @spec create_message(String.t, String.t, boolean) :: {:error, Map.t} | {:ok, Mixcord.Constructs.Message.t}
  def create_message(channel_id, content, tts \\ false) do
    request(:post, Constants.channel_messages(channel_id), %{content: content, tts: tts})
  end

  @doc """
  Send a message to a channel.

  Send `content` to the channel identified with `channel_id`.
  `tts` is an optional parameter that dictates whether the message should be played over text to speech.

  Raises `Mixcord.Errors.ApiError` if error occurs while making the rest call.
  Returns `Mixcord.Constructs.Message` if successful.
  """
  @spec create_message!(String.t, String.t, boolean) :: no_return | Mixcord.Constructs.Message.t
  def create_message!(channel_id, content, tts \\ false) do
    create_message(channel_id, content, tts)
    |> bangify
  end

  @doc """
  Edit a message.

  Edit a message with the given `content`. Message to edit is specified by `channel_id` and `message_id`.

  Returns the edited `{:ok, Mixcord.Constructs.Message}` if successful. `{:error, %{status_code: status_code, message: message}}` otherwise.
  """
  @spec edit_message(String.t, String.t, String.t) :: {:error, Map.t} | {:ok, Mixcord.Constructs.Message.t}
  def edit_message(channel_id, message_id, content) do
    request(:patch, Constants.channel_message(channel_id, message_id), %{content: content})
  end

  @doc """
  Edit a message.

  Edit a message with the given `content`. Message to edit is specified by `channel_id` and `message_id`.

  Raises `Mixcord.Errors.ApiError` if error occurs while making the rest call.
  Returns the edited `Mixcord.Constructs.Message` if successful.
  """
  @spec edit_message!(String.t, String.t, String.t) :: {:error, Map.t} | {:ok, Mixcord.Constructs.Message.t}
  def edit_message!(channel_id, message_id, content) do
    edit_message(channel_id, message_id, content)
    |> bangify
  end

  @doc """
  Delete a message.

  Delete a message specified by `channel_id` and `message_id`.

  Returns `{:ok}` if successful. `{:error, %{status_code: status_code, message: message}}` otherwise.
  """
  @spec delete_message(String.t, String.t) :: {:error, Map.t} | {:ok}
  def delete_message(channel_id, message_id) do
    request(:delete, Constants.channel_message(channel_id, message_id))
  end

  @doc """
  Delete a message.

  Delete a message specified by `channel_id` and `message_id`.

  Raises `Mixcord.Errors.ApiError` if error occurs while making the rest call.
  Returns {:ok} if successful.
  """
  @spec delete_message!(String.t, String.t) :: {:error, Map.t} | {:ok}
  def delete_message!(channel_id, message_id) do
    delete_message(channel_id, message_id)
    |> bangify
  end

  def get_channel(channel_id) do
    request(:get, Constants.channel(channel_id))
  end

  def edit_channel(channel_id, options) do
    request(:patch, Constants.channel(channel_id), options)
  end

  def delete_channel(channel_id) do
    request(:delete, Constants.channel(channel_id))
  end

  def get_channel_messages(channel_id, options) do
    request(:get, Constants.channel_messages(channel_id), options)
  end

  def get_channel_message(channel_id, message_id) do
    request(:get, Constants.channel_message(channel_id, message_id))
  end

  def bulk_delete_messages(channel_id) do
    request(:delete, Constants.channel_bulk_delete(channel_id))
  end

  def edit_channel_permissions(channel_id, overwrite_id) do
    request(:put, Constants.channel_permission(channel_id, overwrite_id))
  end

  def delete_channel_permissions(channel_id, overwrite_id) do
    request(:delete, Constants.channel_permission(channel_id, overwrite_id))
  end

  def get_channel_invites(channel_id) do
    request(:get, Constants.channel_invites(channel_id))
  end

  def create_channel_invite(channel_id, options \\ %{}) do
    request(:post, Constants.channel_invites(channel_id))
  end

  def start_typing(channel_id) do
    request(:post, Constants.channel_typing(channel_id))
  end

  def add_pinned_message(channel_id, message_id) do
    request(:get, Constants.channel_pin(channel_id, message_id))
  end

  def delete_pinned_message(channel_id, message_id) do
    request(:delete, Constants.channel_pin(channel_id, message_id))
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