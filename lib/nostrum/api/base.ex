defmodule Nostrum.Api.Base do
  @moduledoc false

  @version Nostrum.Mixfile.project()[:version]

  import Nostrum.Constants, only: [base_route: 0]

  def request(conn, method, route, body, raw_headers, params) do
    headers = process_request_headers(raw_headers)
    # Convert method from atom to string for `:gun`
    method =
      method
      |> Atom.to_string()
      |> String.upcase()

    query_string = URI.encode_query(params)

    full_route = "#{base_route()}#{route}?#{query_string}"

    stream = :gun.request(conn, method, full_route, headers, process_request_body(body))

    case :gun.await(conn, stream) do
      {:response, :fin, status, headers} ->
        {:ok, {status, headers, ""}}

      {:response, :nofin, status, headers} ->
        {:ok, body} = :gun.await_body(conn, stream)
        {:ok, {status, headers, body}}

      {:error, _reason} = result ->
        result
    end
  end

  def process_request_body(""), do: ""
  def process_request_body({:multipart, content}), do: content
  def process_request_body(body), do: Poison.encode!(body)

  def process_request_headers(headers) do
    user_agent = [
      {"user-agent", "DiscordBot (https://github.com/kraigie/nostrum, #{@version})"} | headers
    ]

    token = "Bot #{Application.get_env(:nostrum, :token)}"

    [{"authorization", token} | user_agent]
  end

  def process_response_body(body) do
    body
  end
end
