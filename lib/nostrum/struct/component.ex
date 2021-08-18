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

  ![Discord Buttons](https://discord.com/assets/7bb017ce52cfd6575e21c058feb3883b.png)

  - Buttons must exist inside an Action Row
  - An Action Row can contain up to 5 buttons
  - An Action Row containing buttons cannot also contain a select menu

  For more information check out the [Discord API Button Styles](https://discord.com/developers/docs/interactions/message-components#button-object-button-styles) for more information.

  ## Link Buttons

  - Link buttons **do not** send an `interaction` to your app when clicked
  - Link buttons **must** have a `url`, and **cannot** have a `custom_id`
  - Link buttons will **always** use `style: 5`

  #### Link `style: 5`
  ![Secondary](https://user-images.githubusercontent.com/34633373/129678527-d8d1c988-33c5-46d6-874e-2cfb2d9ba7f4.png)

  ## Interaction Buttons ( Non-link Buttons )

  > Discord calls these buttons "Non-link Buttons"  due to the fact that they do not contain a url. However it would be more accurate to call them an "Interaction Button" as they **do** fire an interaction when clicked which is far more useful for your applications interactivity. As such they are referred to as "Interaction Button" throughout the rest of this module.

  - Interaction buttons **must** have a `custom_id`, and **cannot** have a `url`
  - Can have one of the below `:style` applied.

  #### Primary `style: 1`
  ![Primary](https://user-images.githubusercontent.com/34633373/129678518-74790396-efcc-45f0-9a84-eba3f4c66950.png)

  #### Secondary `style: 2`
  ![Secondary](https://user-images.githubusercontent.com/34633373/129678527-d8d1c988-33c5-46d6-874e-2cfb2d9ba7f4.png)

  #### Success `style: 3`
  ![Success](https://user-images.githubusercontent.com/34633373/129678543-5d72ba09-f042-49b6-b56a-9ae072011ee1.png)

  #### Danger `style: 4`
  ![Danger (1)](https://user-images.githubusercontent.com/34633373/129678473-7e4b045f-b4a3-4993-96a2-4916cb88161f.png)

  ## 🐼 ~~Emoji Buttons~~

  > Note: The discord documentation and marketing material in relation to buttons indicates that there are three kinds of buttons: 🐼 **Emoji Buttons**, **Link Buttons** & **Non-Link Buttons**. When in fact all buttons can contain an emoji. Because of this reason 🐼 **Emoji Buttons** are not included as a seperate type. Emojis will be instead handled by the two included ( superior ) button types.

  ![emoji buttons in action](http://puu.sh/I4wdq/e6c28f7c85.png)

  > The field requirements are already becoming convoluted especially considering everything so far is all still a "Component". Using the sub types and helper functions will ensure all of the rules are followed when creating components.

  ## Select Menu
  Select menus are another interactive component that renders on messages. On desktop, clicking on a select menu opens a dropdown-style UI; on mobile, tapping a select menu opens up a half-sheet with the options.

  ![Discord Selects](https://discord.com/assets/0845178564ed70a6c657d9b40d1de8fc.png)

  Select menus support single-select and multi-select behavior, meaning you can prompt a user to choose just one item from a list, or multiple. When a user finishes making their choice by clicking out of the dropdown or closing the half-sheet, your app will receive an interaction.
  - Select menus **must** be sent inside an Action Row
  - An Action Row can contain **only one** select menu
  - An Action Row containing a select menu **cannot** also contain buttons
  """

  defmacro __using__(_opts) do
    quote do
      alias Nostrum.Struct.Component
      alias Nostrum.Struct.Component.{ActionRow, Button, SelectMenu, SelectMenuOption}
      alias Nostrum.Struct.Emoji
      alias Nostrum.Util
    end
  end

  alias Nostrum.Struct.Component.{Button, SelectMenu, SelectMenuOption}
  alias Nostrum.Util

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
    :components
  ]

  @type t :: %{
          type: type,
          custom_id: custom_id,
          disabled: disabled,
          style: style,
          label: label,
          emoji: emoji,
          url: url,
          options: options,
          placeholder: placeholder,
          min_values: min_values,
          max_values: max_values,
          components: components
        }

  @typedoc """
  The type of component.

  Valid for All Types.

  | | Component Types |
  |------|-----|
  |  `1` |  Action Row |
  |  `2` |  Button |
  |  `3` |  SelectMenu |

  Check out the [Discord API Message Component Types](https://discord.com/developers/docs/interactions/message-components#component-object-component-types) for more information.

  """
  @type type :: integer()

  @typedoc """
  Used to identify the command when the interraction is sent to you from the user.

  Valid for [Interaction Buttons](#module-interaction-button) & [Select Menus](#module-select-menu).
  """
  @type custom_id :: String.t() | nil

  @typedoc """
  Indicates if the component is disabled or not.

  Valid for [Buttons](#module-buttons) & [Select Menus](#module-select-menu).
  """
  @type disabled :: boolean() | nil

  @typedoc """
  Indicates the style.

  Valid for Valid for [Interaction Buttons](#module-interaction-button) only,
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
  @type emoji :: Emoji.partial_button_emoji() | nil

  @typedoc """
  A url for link buttons.

  Valid for: Buttons, `String.t()` representing	an
  """
  @type url :: String.t() | nil

  @typedoc """
  A list of options for select menus, max 25.

  Valid for [Select Menus](#module-select-menu).
  """
  @type options :: [SelectMenuOption.t()] | nil

  @typedoc """
  Placeholder text if nothing is selected, max 100 characters

  Valid for [Select Menus](#module-select-menu).

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
  A list of components to place inside an action row.

  Due to constraints of action rows, this can either be a list of up to five buttons, or a single select menu.

  Valid for [Action Row](#module-action-row).
  """

  @type components :: [SelectMenu.t() | Button.t() | nil]

  @spec to_struct(map) :: Module.t()
  def to_struct(map) do
    new =
      map
      |> Map.new(fn {k, v} -> {Util.maybe_to_atom(k), v} end)
      |> Map.update(:emoji, nil, &Util.cast(&1, {:struct, Emoji}))
      |> Map.update(:options, nil, &Util.cast(&1, {:list, {:struct, SelectMenuOption}}))
      |> Map.update(:components, nil, &Util.cast(&1, {:list, {:struct, %__MODULE__{}}}))

    %__MODULE__{}
    |> Kernel.struct(new)
  end
end
