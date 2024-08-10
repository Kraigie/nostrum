defmodule Nostrum.Struct.Component.ActionRow do
  @moduledoc """
  Action Rows.
  """
  alias Nostrum.Constants.ComponentType
  @moduledoc since: "0.5.0"
  use Nostrum.Struct.Component

  @defaults %{
    type: ComponentType.action_row(),
    components: []
  }

  @type t :: %Component{
          type: Component.type(),
          components: [Component.components()]
        }

  @doc """
  Create an empty action row.

  Options can be passed as a keyword list. The only supported option is a list of inner components
  """
  def action_row(opts \\ [])

  def action_row(%Component{type: type} = component) when type in [3, 4, 5, 6, 7, 8] do
    [
      {:components, [component]}
    ]
    |> new()
  end

  def action_row([%Component{type: 2} | _] = components),
    do: action_row([{:components, components}])

  def action_row(opts) do
    [
      {:components, opts[:components]}
    ]
    |> new()
  end

  @doc """
  Appends a button to the action row.

  Returns the action row unchanged if there are already 5 buttons or if the action row contains a select menu or text input.
  """
  def append(action_row, button)

  def append(%Component{type: 1, components: [%Component{type: type} | _]} = c, _)
      when type in [3, 4],
      do: c

  def append(%Component{type: 1, components: [%Component{} | _]} = c, %Component{type: 2} = i),
    do: do_append(c, i)

  def append(%Component{type: 1, components: []} = c, %Component{type: 2} = i),
    do: do_append(c, i)

  defp do_append(%Component{components: components} = c, %Component{type: 2} = i) do
    inner_components_count = Enum.count(components)

    cond do
      inner_components_count == 5 ->
        c

      inner_components_count < 5 ->
        update(c, [{:components, components ++ [i]}])
    end
  end

  @doc """
  Lazily appends a button to the action row.

  If there are already 5 buttons, the first one will be dropped.
  """
  def append_lazy(action_row, button)

  def append_lazy(%Component{type: 1, components: [%Component{type: type} | _]} = inner_select, _)
      when type in [3, 4] do
    inner_select
  end

  def append_lazy(
        %Component{type: 1, components: [%Component{} | _]} = action_row_with_buttons,
        %Component{type: 2} = button_to_append
      ) do
    do_append_lazy(action_row_with_buttons, button_to_append)
  end

  def append_lazy(
        %Component{type: 1, components: []} = empty_action_row,
        %Component{type: 2} = button_to_append
      ),
      do: do_append(empty_action_row, button_to_append)

  defp do_append_lazy(
         %Component{components: [_head | tail] = components} = action_row,
         %Component{type: 2} = button_to_append
       ) do
    inner_components_count = Enum.count(components)

    cond do
      inner_components_count == 5 ->
        update(action_row, [{:components, tail ++ [button_to_append]}])

      inner_components_count < 5 ->
        update(action_row, [{:components, components ++ [button_to_append]}])
    end
  end

  @doc """
  Puts the components into the action row unless a list of inner components already exists.
  """

  def put_new(%Component{type: 1, components: []} = component, list_of_components) do
    update(component, [{:components, list_of_components}])
  end

  def put_new(%Component{type: 1, components: [_ | _]} = component, _), do: component

  @doc """
  Puts the given component into the action row, any existing components are discarded.
  """
  def put(%Component{type: 1} = component, %Component{type: type} = select_menu)
      when type in [3, 4] do
    update(component, [{:components, [select_menu]}])
  end

  def put(%Component{type: 1} = component, [%Component{type: type} | []] = select_menu)
      when type in [3, 4] do
    update(component, [{:components, select_menu}])
  end

  def put(%Component{type: 1} = component, [%Component{} | _] = list_of_components) do
    update(component, [{:components, list_of_components}])
  end
end
