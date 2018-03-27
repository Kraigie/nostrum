defmodule Nostrum.Error.ConsumerError do
  @moduledoc """
  Represents an error when interacting with a consumer.

  This occurs likely because incorrect values were returned from the `handle_event`
  callback.
  """

  defexception [:message]

  def exception(found: finding) do
    msg = "Expected {:ok, state}, received: #{inspect(finding)}"
    %__MODULE__{message: msg}
  end

  def exception(msg) do
    %__MODULE__{message: msg}
  end
end
