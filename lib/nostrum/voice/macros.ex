defmodule Nostrum.Voice.Macros do
  @moduledoc false

  alias Nostrum.Constants

  defp opcode_from_atom_name(name) do
    name |> Atom.to_string() |> String.upcase() |> Constants.voice_opcode_from_name() ||
      raise(ArgumentError, "couldn't infer voice opcode for function #{inspect(name)}")
  end

  defmacro def_json_payload({name, _meta, _args} = head, do: body) do
    opcode = opcode_from_atom_name(name)

    quote do
      def unquote(head) do
        Jason.encode_to_iodata!(%{
          op: unquote(opcode),
          d: unquote(body)
        })
      end
    end
  end

  defmacro def_binary_payload({name, _meta, _args} = head, do: body) do
    opcode = opcode_from_atom_name(name)

    quote do
      def unquote(head) do
        <<unquote(opcode), unquote(body)::binary>>
      end
    end
  end

  defmacro def_dispatch_payload({name, _meta, _args} = head, do: body) do
    event_type = name |> Atom.to_string() |> String.upcase() |> String.to_atom()

    quote do
      def unquote(head) do
        %{
          t: unquote(event_type),
          d: unquote(body)
        }
      end
    end
  end
end
