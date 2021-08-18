defmodule Nostrum.Struct.Component.ActionRow do
  use Nostrum.Struct.Component

  @action_row_type 1

  @defaults %{
    type: @action_row_type,
    components: []
  }

  @type t :: %{
          type: Component.type(),
          components: [Component.components()]
        }

  def new(opts \\ []) do
    opts
    |> Enum.into(@defaults)
    |> to_component()
  end

  def to_component(map), do: Component.to_struct(map)
end
