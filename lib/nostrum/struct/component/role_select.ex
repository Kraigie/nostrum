defmodule Nostrum.Struct.Component.RoleSelect do
  @moduledoc """
  Role Select
  """
  alias Nostrum.Constants.ComponentType
  alias Nostrum.Struct.Component.SelectMenu

  @defaults %{
    custom_id: nil,
    placeholder: "",
    min_values: 1,
    disabled: false,
    max_values: 1,
    type: ComponentType.role_select()
  }

  use Nostrum.Struct.Component

  @type t :: SelectMenu.t()

  @type opt ::
          {:custom_id, Component.custom_id()}
          | {:placeholder, Component.placeholder()}
          | {:min_values, Component.min_values()}
          | {:max_values, Component.max_values()}
          | {:disabled, Component.disabled()}

  @type opts :: [opt]

  def role_select(custom_id, opts \\ []) when is_binary(custom_id) do
    [
      custom_id: custom_id,
      disabled: opts[:disabled],
      placeholder: opts[:placeholder],
      min_values: opts[:min_values],
      max_values: opts[:max_values]
    ]
    |> new()
  end
end
