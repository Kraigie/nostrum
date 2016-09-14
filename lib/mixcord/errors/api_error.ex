defmodule Mixcord.Errors.ApiError do
  defexception [:message]

  def exception(message: message, status_code: status_code) do
    msg = "Status Code: #{status_code}: " <> message
    %__MODULE__{message: msg}
  end
end