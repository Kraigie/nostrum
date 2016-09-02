defmodule Mixcord.Rest do
    use HTTPoison.Base
    alias Mixcord.Constants

    defp process_url(url) do
        Constants.base_url <> url
    end

    defp process_request_body(body) do
        body
        |> Poison.encode!
    end

    defp process_request_headers(headers) do
        [{"User-Agent", "DiscordBot (test, test)"} | headers]
        [{"content-type", "application/json"} | headers]
    end

    defp process_response_body(body) do
        body
    end
end