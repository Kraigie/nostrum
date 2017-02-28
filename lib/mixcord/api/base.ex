defmodule Mixcord.Api.Base do
  @moduledoc false

  @version Mixcord.Mixfile.project[:version]
  @token Application.get_env(:mixcord, :token)

  use HTTPoison.Base

  alias Mixcord.Constants

  defp process_url(url) do
    Constants.base_url <> url
  end

  defp process_request_body(""),
    do: ""
  defp process_request_body({:multipart, _} = body),
    do: body
  defp process_request_body(body) when is_list(body),
    do: Enum.into(body, %{}) |> Poison.encode!(body)
  defp process_request_body(body),
    do: Poison.encode!(body)

  defp process_request_headers(headers) do
    user_agent = [{"User-Agent", "DiscordBot (https://github.com/kraigie/mixcord, #{@version})"} | headers]
    auth = [{"Authorization", "Bot #{@token}"} | user_agent]
    auth
  end

  defp process_response_body(body) do
    body
  end

end
