defmodule Nostrum.Struct.Message.Component do
  @moduledoc """
  A component attached to a message.

  Note that the fields present depend on the `t:type/0` of the component object.

  See the [Discord API Component Object
  Documentation](https://discord.com/developers/docs/interactions/message-components#component-object)
  for more information.
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Emoji
  alias Nostrum.Util

  defstruct [
    :type,
    :custom_id,
    :disabled?,
    :style,
    :label,
    :emoji,
    :url,
    :options,
    :placeholder,
    :min_values,
    :max_values,
    :components
  ]

  @typedoc """
  Component type.

  This field is always set.

  ## Values

  - ``1``: Action Row - A container for other components.
  - ``2``: Button - A button object.
  - ``3``: Select Menu - A select menu for picking from choices.

  ## References

  See [Discord Developer Portal: Component
  Types](https://discord.com/developers/docs/interactions/message-components#component-object-component-types).
  """
  @type type :: 1 | 2 | 3

  @typedoc """
  A developer-defined identifier for the component.

  Maximum of 100 characters. Only present for buttons and select menus.
  """
  @type custom_id :: String.t() | nil

  @typedoc """
  Whether the component is disabled.

  Only present for buttons and select menus.
  """
  @type disabled? :: boolean() | nil

  @typedoc """
  An integer representing the style of the button.

  Only present for buttons.

  ## Values

  - ``1``: Primary - blurple, ``custom_id`` required.
  - ``2``: Secondary - grey, ``custom_id`` required.
  - ``3``: Success - green, ``custom_id`` required.
  - ``4``: Danger - red, ``custom_id`` required.
  - ``5``: Link - grey, ``url`` required, navigates to the URL.

  ## References

  See [Discord Developer Portal: Button
  Styles](https://discord.com/developers/docs/interactions/message-components#button-object-button-styles).
  """
  @type style :: 1 | 2 | 3 | 4 | 5 | nil

  @typedoc """
  Text that appears on the button.

  Maximum of 80 characters. Only present for buttons.
  """
  @type label :: String.t() | nil

  @typedoc """
  Partial emoji of the button.

  Only present for buttons. The following fields are set:

  - ``name``
  - ``id``
  - ``animated``
  """
  @type emoji :: Emoji.t() | nil

  @typedoc """
  URL for link-style buttons.

  Only present for buttons.
  """
  @type url :: String.t() | nil

  @typedoc """
  Choices of the select menu.

  Maximum of 25 options. Only present for select menus.

  ## References

  See [Discord Developer Portal: Select Option
  Structure](https://discord.com/developers/docs/interactions/message-components#select-menu-object-select-option-structure).
  """
  @type options ::
          [
            %{
              required(:label) => String.t(),
              required(:value) => String.t(),
              optional(:description) => String.t(),
              optional(:emoji) => %{
                id: Emoji.id(),
                name: Emoji.name(),
                animated: Emoji.animated()
              },
              optional(:default) => boolean()
            }
          ]
          | nil

  @typedoc """
  Custom placeholder text if nothing is selected.

  Maximum of 100 characters. Only present for select menus.
  """
  @type placeholder :: String.t() | nil

  @typedoc """
  Minimum number of items that must be chosen.

  Defaults to ``1``. Minimum of ``0``. Maximum of ``25``. Only present for select menus.
  """
  @type min_values :: 0..25

  @typedoc """
  Maximum number of items that must be chosen.

  Defaults to ``1``. Maximum of ``25``. Only present for select menus.
  """
  @type max_values :: 1..25

  @typedoc """
  Child components for action rows.

  Only present for action rows.
  """
  @type components :: [t()]

  @typedoc "Represents a message component."
  @type t :: %__MODULE__{
          type: type(),
          custom_id: custom_id(),
          disabled?: disabled?(),
          style: style(),
          label: label(),
          emoji: emoji(),
          url: url(),
          options: options(),
          placeholder: placeholder(),
          min_values: min_values(),
          max_values: max_values(),
          components: components()
        }

  @doc false
  @spec to_struct(map()) :: t()
  def to_struct(map) do
    %__MODULE__{
      type: map.type,
      custom_id: map[:custom_id],
      disabled?: map[:disabled],
      style: map[:style],
      label: map[:label],
      emoji: Util.cast(map[:emoji], {:struct, Emoji}),
      url: map[:url],
      options: map[:options],
      placeholder: map[:placeholder],
      min_values: map[:min_values],
      max_values: map[:max_values],
      components: Util.cast(map[:components], {:list, {:struct, __MODULE__}})
    }
  end
end
