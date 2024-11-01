defmodule Nostrum.Api.Adapter do
  @moduledoc false

  @version Nostrum.Mixfile.project()[:version]

  import Nostrum.Constants, only: [base_route: 0]
  require Logger

  @typedoc "An anonymous function that returns the bot token."
  @type wrapped_token :: (-> String.t())

  @type methods :: :get | :post | :put | :delete

  @spec request(
          pid,
          methods(),
          String.t(),
          iodata(),
          [{String.t(), String.t()}],
          Enum.t(),
          wrapped_token()
        ) ::
          :gun.stream_ref()
  def request(conn, method, route, body, raw_headers, params, wrapped_token) do
    headers = process_request_headers(raw_headers, wrapped_token)
    # Convert method from atom to string for `:gun`
    method =
      method
      |> Atom.to_string()
      |> String.upcase()

    query_string = URI.encode_query(params)

    full_route = "#{base_route()}#{route}?#{query_string}"
    headers = finalize_request_headers(headers, body)
    :gun.request(conn, method, full_route, headers, process_request_body(body))
  end

  def finalize_request_headers(headers, ""), do: :proplists.delete("content-type", headers)
  def finalize_request_headers(headers, _body), do: headers
  def process_request_body(""), do: ""
  def process_request_body({:multipart, content}), do: content
  def process_request_body(body), do: Jason.encode_to_iodata!(body)

  def process_request_headers(headers, wrapped_token) do
    [
      {"authorization", "Bot #{wrapped_token.()}"},
      {"user-agent", "DiscordBot (https://github.com/kraigie/nostrum, #{@version})"} | headers
    ]
  end

  def process_response_body(body) do
    body
  end
end
