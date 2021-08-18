defmodule Nostrum.Struct.Component.SelectMenu do
  use Nostrum.Struct.Component

  @defaults %{
    custom_id: nil,
    options: [],
    placeholder: "",
    min_values: 1,
    max_values: 1,
    disabled: false,
    type: 3
  }

  @type custom_id :: Component.custom_id()

  @type t :: %{
          type: Component.type(),
          custom_id: Component.custom_id(),
          disabled: Component.disabled(),
          options: Component.options(),
          placeholder: Component.placeholder(),
          min_values: Component.min_values(),
          max_values: Component.max_values()
        }

  @type opt ::
          {:custom_id, Component.custom_id()}
          | {:options, Component.options()}
          | {:placeholder, Component.placeholder()}
          | {:min_values, Component.min_values()}
          | {:max_values, Component.max_values()}
          | {:disabled, Component.disabled()}

  @type opts :: [opt]

  def new(opts \\ []) do
    opts
    |> Enum.into(@defaults)
    |> to_component()
  end

  def to_component(map), do: Component.to_struct(map)
end
