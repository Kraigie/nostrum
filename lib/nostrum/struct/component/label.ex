defmodule Nostrum.Struct.Component.Label do
  @moduledoc """
  Labels.
  """
  @moduledoc since: "NEXTVERSION"
  alias Nostrum.Constants.ComponentType
  use Nostrum.Struct.Component

  @defaults %{
    type: ComponentType.label(),
    label: "",
    description: nil,
    component: nil
  }

  @type t :: %Component{
          type: Component.type(),
          label: Component.label(),
          description: Component.description(),
          component: Component.component()
        }

  @type opt ::
          {:component, Component.component()}
          | {:label, Component.label()}
          | {:description, Component.description()}

  @type opts :: [opt]

  @doc """
  Create an empty label.

  Options can be passed as a keyword list.
  """
  def label(opts \\ [])

  def label(opts) do
    [
      {:component, opts[:component]},
      {:label, opts[:label]},
      {:description, opts[:description]}
    ]
    |> new()
  end
end
