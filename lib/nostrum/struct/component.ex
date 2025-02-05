defmodule Nostrum.Struct.Component do
  @moduledoc """
  Components are a framework for adding interactive elements to the messages your app or bot sends. They're accessible, customizable, and easy to use. There are several different types of components; this documentation will outline the basics of this new framework and each example.

  > Components have been broken out into individual modules for easy distinction between them and to separate helper functions and individual type checking between component types - especially as more components are added by Discord.

  Each of the components are provided all of the valid types through this module to avoid repetition and allow new components to be added quicker and easier.

  ## Action Row
  An Action Row is a non-interactive container component for other types of components. It has a `type: 1` and a sub-array of `components` of other types.

  - You can have up to 5 Action Rows per message
  - An Action Row cannot contain another Action Row
  - An Action Row containing buttons cannot also contain a select menu

  ## Buttons
  Buttons are interactive components that render on messages. They have a `type: 2`, They can be clicked by users. Buttons in Nostrum are further separated into two types, detailed below. Only the [Interaction Button](#module-interaction-buttons-non-link-buttons) will fire a `Nostrum.Struct.Interaction` when pressed.

  ![Discord Buttons](./assets/buttons.png)

  - Buttons must exist inside an Action Row
  - An Action Row can contain up to 5 buttons
  - An Action Row containing buttons cannot also contain a select menu

  For more information check out the [Discord API Button Styles](https://discord.com/developers/docs/interactions/message-components#button-object-button-styles) for more information.

  ## Link Buttons

  - Link buttons **do not** send an `interaction` to your app when clicked
  - Link buttons **must** have a `url`, and **cannot** have a `custom_id`
  - Link buttons will **always** use `style: 5`

  #### Link `style: 5`
  ![Link Button](./assets/secondary_button.png)

  ## Interaction Buttons ( Non-link Buttons )

  > Discord calls these buttons "Non-link Buttons"  due to the fact that they do not contain a url. However it would be more accurate to call them an "Interaction Button" as they **do** fire an interaction when clicked which is far more useful for your applications interactivity. As such they are referred to as "Interaction Button" throughout the rest of this module.

  - Interaction buttons **must** have a `custom_id`, and **cannot** have a `url`
  - Can have one of the below `:style` applied.

  #### Primary `style: 1`
  ![Primary](./assets/primary_button.png)

  #### Secondary `style: 2`
  ![Secondary](./assets/secondary_button.png)

  #### Success `style: 3`
  ![Success](./assets/success_button.png)

  #### Danger `style: 4`
  ![Danger (1)](./assets/danger_button.png)

  ## 🐼 ~~Emoji Buttons~~

  > Note: The discord documentation and marketing material in relation to buttons indicates that there are three kinds of buttons: 🐼 **Emoji Buttons**, **Link Buttons** & **Non-Link Buttons**. When in fact all buttons can contain an emoji. Because of this reason 🐼 **Emoji Buttons** are not included as a separate type. Emojis will be instead handled by the two included ( superior ) button types.

  ![emoji buttons in action](./assets/emoji_button.png)

  > The field requirements are already becoming convoluted especially considering everything so far is all still a "Component". Using the sub types and helper functions will ensure all of the rules are followed when creating components.

  ## Select Menu
  Select menus are another interactive component that renders on messages. On desktop, clicking on a select menu opens a dropdown-style UI; on mobile, tapping a select menu opens up a half-sheet with the options.

  ![Discord Selects](./assets/select_menu.png)

  Select menus support single-select and multi-select behavior, meaning you can prompt a user to choose just one item from a list, or multiple. When a user finishes making their choice by clicking out of the dropdown or closing the half-sheet, your app will receive an interaction.
  - Select menus **must** be sent inside an Action Row
  - An Action Row can contain **only one** select menu
  - An Action Row containing a select menu **cannot** also contain buttons

  ## Text Input
  Text inputs are an interactive component that render on modals. They can be used to collect short-form or long-form text.
  - Text inputs **must** be sent inside an Action Row
  - An Action Row can contain **only one** text input
  - An Action Row containing a text input **cannot** also contain buttons or a select menu

  Can be used to collect short-form or long-form text.
  - For short-form text, use `style: 1`
  - For long-form text, use `style: 2`

  Text inputs are only allowed to be sent as part of an Interaction response that opens a MODAL.
  """
  @moduledoc since: "0.5.0"

  alias Nostrum.Struct.Channel

  defmacro __using__(_opts) do
    quote do
      alias Nostrum.Struct.Component.{ActionRow, Button, Option, SelectMenu, TextInput}
      alias Nostrum.Struct.{Component, Emoji}
      alias Nostrum.Util
      @before_compile Component
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      alias Nostrum.Struct.Component

      defp new(opts \\ []) do
        @defaults
        |> to_component(opts)
      end

      defp update(%Component{} = component, opts \\ []) do
        component
        |> Map.from_struct()
        |> to_component(opts)
      end

      defp to_component(component_map, opts) do
        opts
        |> Stream.reject(fn {_, v} -> v == nil end)
        |> Enum.into(component_map)
        |> Stream.filter(fn {k, _} -> k in allowed_keys() end)
        |> Enum.into(%{})
        |> flatten()
        |> Component.to_struct()
      end

      defp allowed_keys, do: Map.keys(@defaults)

      ## Destroy all structs and ensure nested map
      def flatten(map), do: :maps.map(&do_flatten/2, map)
      defp do_flatten(_key, value), do: enm(value)
      defp enm(list) when is_list(list), do: Enum.map(list, &enm/1)

      defp enm(%{__struct__: _} = component),
        do: :maps.map(&do_flatten/2, Map.from_struct(component))

      defp enm(data), do: data
    end
  end

  @doc """
  Create a component from the given keyword list of options

  > Note: While using this function directly, you are not guaranteed to produce a valid component and it is the responsibility of the user to ensure they are passing a valid combination of component attributes. eg. if you pass a button component both a `custom_id`, and a `url`, the component is invalid as only one of these fields is allowed.
  """
  @callback new(opts :: [keyword()]) :: t()

  @doc """
  Updates a component with the parameters provided.

  > Note: While using this function directly, you are not guaranteed to produce a valid component and it is the responsibility of the user to ensure they are passing a valid combination of component attributes. eg. if you pass a button component both a `custom_id`, and a `url`, the component is invalid as only one of these fields is allowed.
  """
  @callback update(t(), opts :: [keyword()]) :: t()

  alias Nostrum.Struct.Component.{ActionRow, Button, DefaultValue, Option, SelectMenu, TextInput}
  alias Nostrum.Struct.Emoji
  alias Nostrum.Util

  @derive Jason.Encoder
  defstruct [
    :type,
    :custom_id,
    :disabled,
    :style,
    :label,
    :emoji,
    :url,
    :options,
    :placeholder,
    :min_values,
    :max_values,
    :min_length,
    :max_length,
    :required,
    :value,
    :components,
    :channel_types,
    :default_values
  ]

  @typedoc """
  The currently valid component types.
  """
  @type t :: ActionRow.t() | Button.t() | SelectMenu.t() | TextInput.t()

  @typedoc """
  The type of component.

  Valid for All Types.

  | | Component Types |
  |------|-----|
  |  `1` |  Action Row |
  |  `2` |  Button |
  |  `3` |  SelectMenu |
  |  `4` |  TextInput |

  You can use one of the `Nostrum.Constants.ComponentType` methods.

  Check out the [Discord API Message Component Types](https://discord.com/developers/docs/interactions/message-components#component-object-component-types) for more information.

  """
  @type type :: integer()

  @typedoc """
  Used to identify the component when the interaction is sent to you from the user.

  Valid for [Interaction Buttons](#module-interaction-button), [Select Menus](#module-select-menu), and [Text Input](#module-text-input).
  """
  @type custom_id :: String.t() | nil

  @typedoc """
  Indicates if the component is disabled or not.

  Valid for [Buttons](#module-buttons) & [Select Menus](#module-select-menu).
  """
  @type disabled :: boolean() | nil

  @typedoc """
  Indicates the style.

  You can use one of the `Nostrum.Constants.ButtonStyle` methods.

  Valid for Valid for [Interaction Buttons](#module-interaction-button) and [Text Input](#module-text-input).
  """
  @type style :: integer() | nil

  @typedoc """
  A string that appears on the button, max 80 characters.

  Valid for [Buttons](#module-buttons)
  """
  @type label :: String.t() | nil

  @typedoc """
  A partial emoji to display on the object.

  Valid for [Buttons](#module-buttons)
  """
  @type emoji :: %{id: Emoji.id(), name: Emoji.name(), animated: Emoji.animated()} | nil

  @typedoc """
  A url for link buttons.

  Valid for: [Buttons](#module-buttons)
  """
  @type url :: String.t() | nil

  @typedoc """
  A list of options for select menus, max 25.

  Valid for [Select Menus](#module-select-menu).
  """
  @type options :: [Option.t()] | nil

  @typedoc """
  Placeholder text if nothing is selected, max 100 characters

  Valid for [Select Menus](#module-select-menu) and [Text Input](#module-text-input).

  """
  @type placeholder :: String.t() | nil

  @typedoc """
  The minimum number of permitted selections. Minimum value 0, max 25.

  Valid for [Select Menus](#module-select-menu).
  """
  @type min_values :: integer() | nil

  @typedoc """
  The maximum number of permitted selections. Minimum value 0, max 25.

  Valid for [Select Menus](#module-select-menu).
  """
  @type max_values :: integer() | nil

  @typedoc """
  The minimum length of the text input. Minimum value 0, max 4000.

  Valid for [Text Input](#module-text-input).
  """
  @typedoc since: "0.5.1"
  @type min_length :: integer() | nil

  @typedoc """
  The maximum length of the text input. Minimum value 1, max 4000.

  Valid for [Text Input](#module-text-input).
  """
  @typedoc since: "0.5.1"
  @type max_length :: integer() | nil

  @typedoc """
  Indicates if the text input is required.

  Valid for [Text Input](#module-text-input).
  """
  @typedoc since: "0.5.1"
  @type required :: boolean() | nil

  @typedoc """
  A pre-filled value for the text input, max 4000 characters.

  Valid for [Text Input](#module-text-input).
  """
  @typedoc since: "0.5.1"
  @type value :: String.t() | nil

  @typedoc """
  A list of components to place inside an action row.

  Due to constraints of action rows, this can either be a list of up to five buttons, a single select menu, or a single text input.

  Valid for [Action Row](#module-action-row).
  """
  @type components :: [SelectMenu.t() | Button.t() | nil]

  @typedoc since: "0.10.1"
  @type channel_types :: [Channel.type()]

  @typedoc since: "0.10.1"
  @type default_values :: [DefaultValue.t()]

  @spec to_struct(map()) :: struct
  def to_struct(%{} = map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:options, nil, &Util.cast(&1, {:list, {:struct, Option}}))
      |> Map.update(:components, nil, &Util.cast(&1, {:list, {:struct, __MODULE__}}))

    %__MODULE__{}
    |> Kernel.struct(new)
  end
end
