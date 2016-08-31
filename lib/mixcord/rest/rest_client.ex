defmodule Mixcord.Rest_Client do
    alias Mixcord.Constants
    alias Mixcord.Rest

    def init(token) do
        HTTPoison.start
        Agent.start(fn -> token end, name: :token)
    end

    def create_message(channel_id, content, tts \\ false) do
        case request(:post, Constants.channel_messages(channel_id), %{"content" => content, "tts" => tts}) do
            {:error, message: message} ->
                {:error, message: message}
            {:ok, body: body} ->
                {:ok, message: Poison.decode!(~s(body), as: %Message{})}
        end

    end

    def request(type, url, body, options \\ []) do
        format_response Rest.request(type, url, body, [{"Authorization", "Bot #{token}"}], options)
    end

    def format_response(response) do
        case response do
            {:error, %HTTPoison.Error{reason: reason}} ->
                {:error, message: reason}
            {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
                {:ok, body: body}
            {:ok, %HTTPoison.Response{status_code: status_code, body: body}} ->
                {:error, status_code: status_code, message: body}
        end
    end

    def token() do
        Agent.get(:token, &(&1))
    end

end