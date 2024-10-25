defmodule Nostrum.Api.Sticker do
  alias Nostrum.Api
  alias Nostrum.Util
  alias Nostrum.Snowflake
  alias Nostrum.Constants
  alias Nostrum.Struct.Sticker
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.Guild.AuditLogEntry

  @crlf "\r\n"

  @doc ~S"""
  Create a sticker in a guild.

  Every guild has five free sticker slots by default, and each Boost level will
  grant access to more slots.

  Uploaded stickers are constrained to 5 seconds in length for animated stickers, and 320 x 320 pixels.

  Stickers in the [Lottie file format](https://airbnb.design/lottie/) can only
  be uploaded on guilds that have either the `VERIFIED` and/or the `PARTNERED`
  guild feature.

  ## Parameters

  - `name`: Name of the sticker (2-30 characters)
  - `description`: Description of the sticker (2-100 characters)
  - `tags`: Autocomplete/suggestion tags for the sticker (max 200 characters)
  - `file`: A path to a file to upload or a map of `name` (file name) and `body` (file data).
  - `reason` (optional): audit log reason to attach to this event

  ## Returns

  Returns a `t:Nostrum.Struct.Sticker.t/0` on success.
  """
  @doc since: "0.10.0"
  @spec create(
          Guild.id(),
          Sticker.name(),
          Sticker.description(),
          Sticker.tags(),
          String.t() | %{body: iodata(), name: String.t()},
          AuditLogEntry.reason()
        ) :: {:ok, Sticker.t()} | Api.error()
  def create(guild_id, name, description, tags, file, reason \\ nil) do
    opts = %{
      name: name,
      description: description,
      tags: tags
    }

    boundary = Api.generate_boundary()

    multipart = Api.create_multipart([], Jason.encode_to_iodata!(opts), boundary)

    headers =
      Api.maybe_add_reason(reason, [
        {"content-type", "multipart/form-data; boundary=#{boundary}"}
      ])

    file = Api.create_file_part_for_multipart(file, nil, boundary, "file")

    %{
      method: :post,
      route: Constants.guild_stickers(guild_id),
      body:
        {:multipart,
         [
           ~s|--#{boundary}#{@crlf}|,
           file
           | multipart
         ]},
      params: [],
      headers: headers
    }
    |> Api.request()
    |> Api.handle_request_with_decode({:struct, Sticker})
  end

  @doc ~S"""
  Delete a guild sticker with the specified ID.
  """
  @doc since: "0.10.0"
  @spec delete(Guild.id(), Sticker.id()) :: {:ok} | Api.error()
  def delete(guild_id, sticker_id) do
    Api.request(:delete, Constants.guild_sticker(guild_id, sticker_id))
  end

  @doc ~S"""
  Fetch a sticker with the provided ID.

  Returns a `t:Nostrum.Struct.Sticker.t/0`.
  """
  @doc since: "0.10.0"
  @spec get(Sticker.id()) :: {:ok, Sticker.t()} | Api.error()
  def get(sticker_id) do
    Api.request(:get, Constants.sticker(sticker_id))
    |> Api.handle_request_with_decode({:struct, Sticker})
  end

  @doc ~S"""
  Return the specified sticker from the specified guild.

  Returns a `t:Nostrum.Struct.Sticker.t/0`.
  """
  @doc since: "0.10.0"
  @spec get(Guild.id(), Sticker.id()) :: Sticker.t() | Api.error()
  def get(guild_id, sticker_id) do
    Api.request(:get, Constants.guild_sticker(guild_id, sticker_id))
    |> Api.handle_request_with_decode({:struct, Sticker})
  end

  @doc ~S"""
  Fetch a sticker pack with the provided ID.

  Returns a `t:Nostrum.Struct.Sticker.Pack.t/0`.
  """
  @doc since: "0.11.0"
  @spec pack(Snowflake.t()) :: {:ok, Sticker.Pack.t()} | Api.error()
  def pack(id) do
    Api.request(:get, Constants.sticker_pack(id))
    |> Api.handle_request_with_decode({:struct, Sticker.Pack})
  end

  @doc ~S"""
  Get a list of available sticker packs.
  """
  @doc since: "0.10.0"
  @spec packs() :: {:ok, [Sticker.Pack.t()]} | Api.error()
  def packs do
    Api.request(:get, Constants.sticker_packs())
    |> Api.handle_request_with_decode()
    |> case do
      {:ok, %{sticker_packs: packs}} -> {:ok, Util.cast(packs, {:list, {:struct, Sticker.Pack}})}
      resp -> resp
    end
  end

  @doc ~S"""
  List all stickers in the provided guild.

  Returns a list of `t:Nostrum.Struct.Sticker.t/0`.
  """
  @doc since: "0.10.0"
  @spec list(Guild.id()) :: {:ok, [Sticker.t()]} | Api.error()
  def list(guild_id) do
    Api.request(:get, Constants.guild_stickers(guild_id))
    |> Api.handle_request_with_decode({:list, {:struct, Sticker}})
  end

  @doc ~S"""
  Modify a guild sticker with the specified ID.

  Pass in a map of properties to update, with any of the following keys:

  - `name`: Name of the sticker (2-30 characters)
  - `description`: Description of the sticker (2-100 characters)
  - `tags`: Autocomplete/suggestion tags for the sticker (max 200 characters)

  Returns an updated sticker on update completion.
  """
  @doc since: "0.10.0"
  @spec modify(Guild.id(), Sticker.id(), %{
          name: Sticker.name() | nil,
          description: Sticker.description() | nil,
          tags: Sticker.tags() | nil
        }) :: {:ok, Sticker.t()} | Api.error()
  def modify(guild_id, sticker_id, options) do
    Api.request(:patch, Constants.guild_sticker(guild_id, sticker_id), options)
    |> Api.handle_request_with_decode({:struct, Sticker})
  end
end
