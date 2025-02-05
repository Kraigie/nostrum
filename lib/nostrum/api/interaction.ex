defmodule Nostrum.Api.Interaction do
  @moduledoc """
  Functions for interacting with the Discord API's interaction endpoints.

  See: https://discord.com/developers/docs/interactions/overview
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Api.Webhook
  alias Nostrum.Cache.Me
  alias Nostrum.Constants
  alias Nostrum.Struct.Interaction
  alias Nostrum.Struct.Message
  alias Nostrum.Struct.User

  @doc """
  Create a followup message for an interaction.

  Delegates to ``execute_webhook/3``, see the function for more details.
  """
  @spec create_followup_message(User.id(), Interaction.token(), map()) ::
          {:ok, Message.t()} | Api.error()
  def create_followup_message(application_id \\ Me.get().id, token, webhook_payload) do
    Webhook.execute(application_id, token, webhook_payload)
  end

  @doc """
  Same as `create_response/3`, but directly takes the
  `t:Nostrum.Struct.Interaction.t/0` received from the gateway.
  """
  @spec create_response(Interaction.t(), map()) :: {:ok} | Api.error()
  def create_response(interaction, response) do
    create_response(interaction.id, interaction.token, response)
  end

  @doc """
  Create a response to an interaction received from the gateway.

  ## Parameters
  - `id`: The interaction ID to which the response should be created.
  - `token`: The interaction token.
  - `response`: An [`InteractionResponse`](https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-response-object)
    object. See the linked documentation.


  ### Attachments
  To include attachments in the response, you can include a `:files` field in the response.
  This field expects a list of attachments which can be in either of the following formats:
  - A path to the file to upload.
  - A map with the following fields:
    - `:body` The file contents.
    - `:name` The filename of the file.

  ## Example

  ```elixir
  response = %{
    type: 4,
    data: %{
      content: "I copy and pasted this code."
    }
  }
  Nostrum.Api.Interaction.create_response(interaction, response)
  ```

  As an alternative to passing the interaction ID and token, the
  original `t:Nostrum.Struct.Interaction.t/0` can also be passed
  directly. See `create_response/2`.
  """
  @spec create_response(Interaction.id(), Interaction.token(), map()) :: {:ok} | Api.error()
  def create_response(id, token, response) do
    Api.request(
      :post,
      Constants.interaction_callback(id, token),
      Helpers.combine_embeds(response)
      |> Helpers.combine_files()
    )
  end

  @doc """
  Delete a followup message for an interaction.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `token`: Interaction token.
  - `message_id`: Followup message ID.
  """
  @spec delete_followup_message(User.id(), Interaction.token(), Message.id()) ::
          {:ok} | Api.error()
  def delete_followup_message(
        application_id \\ Me.get().id,
        token,
        message_id
      ) do
    Api.request(
      :delete,
      Constants.interaction_followup_message(application_id, token, message_id)
    )
  end

  @doc """
  Same as `delete_response/3`, but directly takes the
  `t:Nostrum.Struct.Interaction.t/0` received from the gateway.
  """
  @doc since: "1.x.x"
  @spec delete_response(Interaction.t()) :: {:ok} | Api.error()
  def delete_response(%Interaction{application_id: application_id, token: token}) do
    delete_response(application_id, token)
  end

  @doc """
  Deletes the original interaction response.
  """
  @doc since: "1.x.x"
  @spec delete_response(User.id(), Interaction.token()) :: {:ok} | Api.error()
  def delete_response(id \\ Me.get().id, token) do
    Api.request(:delete, Constants.interaction_callback_original(id, token))
  end

  @doc """
  Same as `edit_response/3`, but directly takes the
  `t:Nostrum.Struct.Interaction.t/0` received from the gateway.
  """
  @doc since: "1.x.x"
  @spec edit_response(Interaction.t(), map()) :: {:ok, Message.t()} | Api.error()
  def edit_response(%Interaction{application_id: application_id, token: token}, response) do
    edit_response(application_id, token, response)
  end

  @doc """
  Edits the original interaction response.

  Functions the same as `edit_webhook_message/3`
  """
  @doc since: "1.x.x"
  @spec edit_response(User.id(), Interaction.token(), map()) ::
          {:ok, Message.t()} | Api.error()
  def edit_response(id \\ Me.get().id, token, response) do
    Api.request(
      :patch,
      Constants.interaction_callback_original(id, token),
      Helpers.combine_embeds(response)
      |> Helpers.combine_files()
    )
    |> Helpers.handle_request_with_decode({:struct, Message})
  end

  @doc """
  Retrieves the original message of an interaction.
  """
  @doc since: "1.x.x"
  @spec original_response(Interaction.t()) :: Api.error() | {:ok, Message.t()}
  def original_response(%Interaction{application_id: application_id, token: token}) do
    original_response(application_id, token)
  end

  @doc """
  Retrieves the original message of an interaction.
  """
  @doc since: "1.x.x"
  @spec original_response(Interaction.t()) :: Api.error() | {:ok, Message.t()}
  def original_response(id \\ Me.get().id, token) do
    Api.request(:get, Constants.original_interaction_response(id, token))
    |> Helpers.handle_request_with_decode({:struct, Message})
  end
end
