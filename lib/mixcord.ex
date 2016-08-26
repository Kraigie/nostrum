defmodule Mixcord do
    #message = %Message{id: 1111, username: "Ugly Betty"}
    #IO.inspect(message.id)

    Discord.start
    case Discord.get("/channels/179679229036724225") do
        {:ok, %HTTPoison.Response{status_code: status, body: body}} ->
            IO.inspect body
            IO.inspect status
        {:error, %HTTPoison.Error{reason: reason}} ->
            IO.inspect reason
    end

end
