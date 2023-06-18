defmodule Nostrum.Api.Promise do
  @moduledoc """
  Defines an opaque handle returned by each async api call.
  """

  defstruct [:request_id, :handle, :callback]

  @typedoc """
  A promise is a handle to an async api call.
  Callbacks are used to handle decoding the response.

  This struct should be considered opaque by the user.
  """
  @type t(inner_type) :: %__MODULE__{
          callback: (term() -> inner_type)
        }

  @type t :: t(term)

  defimpl Inspect do
    @impl true
    def inspect(%Nostrum.Api.Promise{handle: handle}, options) do
      # since this type is opaque, lets just show the reference
      Inspect.Reference.inspect(handle, options)
    end
  end
end
