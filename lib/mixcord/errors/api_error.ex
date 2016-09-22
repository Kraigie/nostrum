defmodule Mixcord.Errors.ApiError do
  defexception [:message]

  def exception(status_code: status_code, message: message) when is_atom(message) do
    msg = "ERROR: #{status_code} #{to_string(message)}"
    %__MODULE__{message: msg}
  end

  def exception(status_code: status_code, message: message) when is_binary(message) do
    msg = "ERROR: #{status_code} #{to_string(message)}"
    %__MODULE__{message: msg}
  end

end