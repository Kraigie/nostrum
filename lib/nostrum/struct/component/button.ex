defmodule Nostrum.Struct.Component.Button do
  use Nostrum.Struct.Component

  @defaults %{
    button_type: 2,
    style: 1,
    label: "",
    emoji: %{},
    disabled: false,
    url: nil,
    custom_id: nil
  }

  @type link_button :: %{
          type: Component.type(),
          style: Component.style(),
          label: Component.label(),
          emoji: Component.emoji(),
          url: Component.url(),
          disabled: Component.disabled()
        }

  @type interaction_button :: %{
          type: Component.type(),
          style: Component.style(),
          label: Component.label(),
          emoji: Component.emoji(),
          custom_id: Component.custom_id(),
          disabled: Component.disabled()
        }

  @type t :: link_button | interaction_button

  @spec new(any) :: struct
  def new(opts \\ []) do
    opts
    |> Enum.into(@defaults)
    |> to_component()
  end

  def to_component(map), do: Component.to_struct(map)
end
