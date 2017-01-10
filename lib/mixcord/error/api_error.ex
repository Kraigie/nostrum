defmodule Mixcord.Error.ApiError do
  @moduledoc """
  Represents a failed response from the API.

  This occurs when hackney or HTTPoison fail, or when the API doesn't respond with `200` or `204`.
  This should only occur when using the banged API methods.
  """

  @typedoc """
  Map representing the error returned by an API call.

    * status_code
      * `nil` if HTTPoison or Hackney throws an error.
      * Status code of response otherwise.
    * message
      * Error message of response. If the error is from the Discord API,
        this will be a map containing the keys `code` and `message` as strings.
  """
  @type t :: %{
    status_code: integer | nil,
    message: String.t | message_map
  }

  @type message_map :: %{
    code: String.t,
    message: String.t
  }

  # TODO: Don't use just message, use status_code too, will need to define something else
  # in here, as well as return that from the exception methods
  # See Here: https://github.com/edgurgel/httpoison/blob/108a298984a2b814f77f98de08d61e7ac46fdc65/lib/httpoison.ex#L40
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
    msg = "ERROR: #{status_code} #{inspect resp}"
    %__MODULE__{message: msg}
  end

end
