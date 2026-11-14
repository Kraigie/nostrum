defmodule Nostrum.Api.ApplicationEmoji do
  @moduledoc "See: https://discord.com/developers/docs/resources/emoji"
  @moduledoc since: "1.x.x"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Cache.Me
  alias Nostrum.Constants
  alias Nostrum.Struct.Emoji
  alias Nostrum.Struct.User

  @doc since: "1.x.x"
  @spec list(User.id()) :: Api.error() | {:ok, [Emoji.t()]}
  def list(application_id \\ Me.get().id) do
    with {:ok, %{items: items}} <-
           Api.request(:get, Constants.application_emojis(application_id))
           |> Helpers.handle_request_with_decode() do
      {:ok, Enum.map(items, &Emoji.to_struct/1)}
    end
  end

  @doc since: "1.x.x"
  @spec get(User.id(), Emoji.id()) :: Api.error() | {:ok, Emoji.t()}
  def get(application_id \\ Me.get().id, emoji_id) do
    Api.request(:get, Constants.application_emoji(application_id, emoji_id))
    |> Helpers.handle_request_with_decode({:struct, Emoji})
  end

  @doc """
  `image` is a base64 data URI, e.g. `"data:image/png;base64,..."`.
  """
  @doc since: "1.x.x"
  @spec create(User.id(), String.t(), String.t()) :: Api.error() | {:ok, Emoji.t()}
  def create(application_id \\ Me.get().id, name, image) do
    Api.request(:post, Constants.application_emojis(application_id), %{
      name: name,
      image: image
    })
    |> Helpers.handle_request_with_decode({:struct, Emoji})
  end

  @doc since: "1.x.x"
  @spec modify(User.id(), Emoji.id(), String.t()) :: Api.error() | {:ok, Emoji.t()}
  def modify(application_id \\ Me.get().id, emoji_id, name) do
    Api.request(:patch, Constants.application_emoji(application_id, emoji_id), %{name: name})
    |> Helpers.handle_request_with_decode({:struct, Emoji})
  end

  @doc since: "1.x.x"
  @spec delete(User.id(), Emoji.id()) :: Api.error() | :ok
  def delete(application_id \\ Me.get().id, emoji_id) do
    Api.request(:delete, Constants.application_emoji(application_id, emoji_id))
  end
end
