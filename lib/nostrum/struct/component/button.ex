defmodule Nostrum.Struct.Component.Button do
  @moduledoc """
  Helpers for dealing with Button Components
  """
  alias Nostrum.Constants.ButtonStyle
  alias Nostrum.Constants.ComponentType
  @moduledoc since: "0.5.0"
  use Nostrum.Struct.Component

  @type link_button :: %Component{
          type: Component.type(),
          style: Component.style(),
          label: Component.label(),
          emoji: Component.emoji(),
          url: Component.url(),
          disabled: Component.disabled()
        }

  @type interaction_button :: %Component{
          type: Component.type(),
          style: Component.style(),
          label: Component.label(),
          emoji: Component.emoji(),
          custom_id: Component.custom_id(),
          disabled: Component.disabled()
        }

  @type t :: link_button | interaction_button

  @type opt ::
          {:style, Component.style()}
          | {:label, Component.label()}
          | {:emoji, Component.emoji()}
          | {:custom_id, Component.custom_id()}
          | {:disabled, Component.disabled()}
          | {:url, Component.url()}

  @type opts :: [opt]

  @defaults %{
    type: ComponentType.button(),
    style: ButtonStyle.primary(),
    label: "",
    emoji: nil,
    disabled: false,
    url: nil,
    custom_id: nil
  }

  @doc """
  Create a button.

  This function provides direct access to the `new/1` callback.

  > Note: While using this function directly you are **not** guaranteed to return a valid button, providing a valid combination of options becomes the responsibility of the reader. It is instead recommended to use the helper functions provided below.

  """
  def button(opts \\ []) do
    [
      {:style, opts[:style]},
      {:label, opts[:label]},
      {:emoji, opts[:emoji]},
      {:custom_id, opts[:custom_id]},
      {:disabled, opts[:disabled]},
      {:url, opts[:url]}
    ]
    |> new()
  end

  @doc """
  Create an interaction button.

  Read more about interaction buttons in the `Nostrum.Struct.Component` documentation.
  """

  def interaction_button(label, custom_id, opts \\ []) do
    [
      {:label, label},
      {:custom_id, custom_id},
      {:style, opts[:style]},
      {:emoji, opts[:emoji]},
      {:disabled, opts[:disabled]}
    ]
    |> new()
  end

  @doc """
  Create a link button.

  Read more about link buttons in the `Nostrum.Struct.Component` documentation.
  """

  def link_button(label, url, opts \\ []) do
    [
      {:label, label},
      {:url, url},
      {:emoji, opts[:emoji]},
      {:style, ButtonStyle.link()}
    ]
    |> new()
  end

  @doc """
  Toggle the buttons disabled state.
  """
  def toggle(%{type: 2, disabled: button_state} = button) do
    button
    |> struct([{:disabled, !button_state}])
  end

  def toggle(_), do: buttons_only()

  @doc """
  Disables the button when `disabled` is true. Enables it otherwise.
  """
  def disable(%{type: 2} = button, disabled) do
    button
    |> update([{:disabled, disabled}])
  end

  @doc """
  Changes the style of the button.
  """
  def put_style(%{type: 2} = button, style) do
    button
    |> update([{:style, style}])
  end

  def put_style(_, _), do: buttons_only()

  defp buttons_only, do: raise("This operation is only available to button components")
end
