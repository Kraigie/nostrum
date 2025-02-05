defmodule Nostrum.Struct.Component.ChannelSelect do
  @moduledoc """
  Channel Select
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Constants.ComponentType

  @defaults %{
    custom_id: nil,
    min_values: 1,
    disabled: false,
    max_values: 1,
    channel_types: [],
    type: ComponentType.channel_select()
  }

  use Nostrum.Struct.Component

  @type t :: %Component{
          custom_id: Component.custom_id(),
          disabled: Component.disabled(),
          min_values: Component.min_values(),
          max_values: Component.max_values(),
          channel_types: Component.channel_types(),
          type: Component.type()
        }

  @type opt ::
          {:custom_id, Component.custom_id()}
          | {:min_values, Component.min_values()}
          | {:max_values, Component.max_values()}
          | {:disabled, Component.disabled()}
          | {:channel_types, Component.channel_types()}

  @type opts :: [opt]

  def channel_select(custom_id, opts \\ []) when is_binary(custom_id) do
    [
      custom_id: custom_id,
      disabled: opts[:disabled],
      min_values: opts[:min_values],
      max_values: opts[:max_values],
      channel_types: opts[:channel_types]
    ]
    |> new()
  end
end
