defmodule Nostrum.Api.Base do
  @moduledoc false

  @version Nostrum.Mixfile.project()[:version]

  use HTTPoison.Base

  alias Nostrum.Constants

  def process_url(url) do
    URI.encode(Constants.base_url() <> url)
  end

  def process_request_body(""), do: ""
  def process_request_body({:multipart, _} = body), do: body
  def process_request_body(body), do: Poison.encode!(body)

  def process_request_headers(headers) do
    user_agent = [
      {"User-Agent", "DiscordBot (https://github.com/kraigie/nostrum, #{@version})"} | headers
    ]

    token = "Bot " <> Application.get_env(:nostrum, :token)

    [{"Authorization", token} | user_agent]
  end

  def process_response_body(body) do
    body
  end
end
