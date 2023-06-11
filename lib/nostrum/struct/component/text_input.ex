defmodule Nostrum.Struct.Component.TextInput do
  @moduledoc """
  Text Input.
  """
  alias Nostrum.Constants.ComponentType
  alias Nostrum.Constants.TextInputStyle

  @moduledoc since: "0.5.1"
  use Nostrum.Struct.Component

  @type t :: %Component{
          type: Component.type(),
          custom_id: Component.custom_id(),
          style: Component.style(),
          label: Component.label(),
          placeholder: Component.placeholder(),
          min_length: Component.min_length(),
          max_length: Component.max_length(),
          value: Component.value()
        }

  @type opt ::
          {:style, Component.style()}
          | {:min_length, Component.min_length()}
          | {:max_length, Component.max_length()}
          | {:value, Component.value()}
          | {:placeholder, Component.placeholder()}
          | {:required, Component.required()}
          | {:value, Component.value()}

  @type opts :: [opt]

  @defaults %{
    type: ComponentType.text_input(),
    custom_id: nil,
    style: TextInputStyle.short(),
    label: "",
    min_length: nil,
    max_length: nil,
    required: false,
    value: nil,
    placeholder: nil
  }

  @doc """
  Create a text input component.
  """
  def text_input(label, custom_id, opts \\ []) do
    [
      {:label, label},
      {:custom_id, custom_id},
      {:style, opts[:style]},
      {:min_length, opts[:min_length]},
      {:max_length, opts[:max_length]},
      {:required, opts[:required]},
      {:value, opts[:value]},
      {:placeholder, opts[:placeholder]}
    ]
    |> new()
  end

  def put_style(%{type: 4} = text_input, style) do
    text_input
    |> update([{:style, style}])
  end
end
