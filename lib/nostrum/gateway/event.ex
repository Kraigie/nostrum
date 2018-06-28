defmodule Nostrum.Gateway.Event do
  alias Nostrum.Gateway.{HelloEvent, ReadyEvent}

  @type t :: ReadyEvent.t()

  @spec new(Enum.t()) :: {:ok, struct} | :error
  def new(enum) do
    string_map =
      enum
      |> Enum.map(fn {k, v} -> {to_string(k), v} end)
      |> Map.new()

    type = to_string(string_map["t"])

    build_event(type, string_map)
  end

  defp build_event(type, enum)
  defp build_event("HELLO", enum), do: {:ok, HelloEvent.new(enum)}
  defp build_event("READY", enum), do: {:ok, ReadyEvent.new(enum)}
  defp build_event(_, _), do: :error
end
