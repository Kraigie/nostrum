defmodule Mixcord.RestClient do
  @moduledoc """
  Interface for Discord's rest API.
  """
  alias Mixcord.Constants
  alias Mixcord.Constructs.Message
  alias Mixcord.Constructs.User
  alias Mixcord.Rest

  @doc """
  Starts the rest client and initializes the agent containing your API `token`.
  """
  @spec token(String.t) :: nil
  def init(token) do
    HTTPoison.start
    Agent.start(fn -> token end, name: :token)
  end

  @doc """
  Sends `content` to the channel identified with `channel_id`.
  `tts` is an optional parameter that dictates whether the message should be played over text to speech.

  Returns `{:ok, Mixcord.Constructs.Message}` if successful. `{:error, reason}` otherwise.
  """
  @spec create_message(String.t, String.t, boolean) :: {:error, String.t} | {:ok, Mixcord.Constructs.Message.t}
  def create_message(channel_id, content, tts \\ false) do
    case request(:post, Constants.channel_messages(channel_id), %{"content" => content, "tts" => tts}) do
      {:error, message: message} ->
        {:error, message}
      {:ok, body: body} ->
        {:ok, Poison.decode!(body, as: %Message{author: %User{}})}
        #https://github.com/devinus/poison/issues/32#issuecomment-172021478
    end
  end

  defp request(type, url, body, options \\ []) do
    format_response(Rest.request(type, url, body, [{"Authorization", "Bot #{token}"}], options))
  end

  defp format_response(response) do
    case response do
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, message: reason}
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body: body}
      {:ok, %HTTPoison.Response{status_code: status_code, body: body}} ->
        {:error, status_code: status_code, message: body}
    end
  end

  @doc """
  Returns the token of the bot
  """
  def token() do
    Agent.get(:token, &(&1))
  end

end