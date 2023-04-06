defmodule Nostrum.Struct.Component.SelectMenu do
  @moduledoc """
  Select Menu
  """
  alias Nostrum.Constants.ComponentType
  @moduledoc since: "0.5.0"
  @defaults %{
    custom_id: nil,
    options: [],
    placeholder: "",
    min_values: 1,
    disabled: false,
    max_values: 1,
    type: ComponentType.string_select()
  }
  use Nostrum.Struct.Component

  @type t :: %Component{
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

  @doc """
  Creates a select menu that can be used inside an action row.

  Options can be passed as a keyword list.

  ## Parameters

  - `custom_id` - lower case string, used for matching against when your application receives an interaction.

  ## Options

  - `disabled` - If the select should be disabled
  - `options` - A list of options for the select menu, see `Nostrum.Struct.Component.Option`
  - `placeholder` - Value to be shown before anything is selected
  - `min_values` - minimum number of values the user must select, between 0 and 25, default is 1
  - `max_values` - maximum number of values the user must select, between 0 and 25, default is 1

  """
  def select_menu(custom_id, opts \\ []) when is_binary(custom_id) do
    [
      {:custom_id, custom_id},
      {:disabled, opts[:disabled]},
      {:options, opts[:options]},
      {:placeholder, opts[:placeholder]},
      {:min_values, opts[:min_values]},
      {:max_values, opts[:max_values]}
    ]
    |> new()
  end
end
