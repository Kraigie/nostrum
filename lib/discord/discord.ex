defmodule Discord do
    defstruct [add token here, sort of makes a class]
    use HTTPoison.Base

    @base_url "https://discordapp.com/api/v6"

    def process_url(url) do
        @base_url <> url
    end

    defp process_request_headers(headers) do
        headers ++ ["Authorization": "Bot TOKEN HERE", "User-Agent": "DiscordBot (test, test)"]
    end

    def process_response_body(body) do
        body
        |> Poison.decode!
    end

end