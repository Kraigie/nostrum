defmodule Mixcord.Error.ApiError do
  @moduledoc """
  Represents a failed response from the API.

  This occurs when hackney or HTTPoison fail, or when the API doesn't respond with `200` or `204`.

  Represented as a map that is structred as follows:
    * status_code
      * `nil` if HTTPoison or Hackney throws an error.
      * Status code of response otherwise.
    * message
      * Error message of response. If the error is from the Discord API,
      this will be a map containing the keys `code` and `message` as strings.
  """

  defexception [:message]

  def exception(status_code: status_code, message: message) when is_atom(message) do
    msg = "ERROR: #{status_code} #{to_string(message)}"
    %__MODULE__{message: msg}
  end

  def exception(status_code: status_code, message: message) when is_binary(message) do
    msg = "ERROR: #{status_code} #{message}"
    %__MODULE__{message: msg}
  end

  def exception(status_code: status_code, message: resp) when is_map(resp) do
    msg = "ERROR: #{status_code} #{resp["message"]} #{resp["code"]}"
    %__MODULE__{message: msg}
  end

end