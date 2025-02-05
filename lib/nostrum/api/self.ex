defmodule Nostrum.Api.Self do
  @moduledoc """
  Module for interacting with the current user.

  See the endpoints containing @me in the Discord API documentation: https://discord.com/developers/docs/resources/user
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Shard.Session
  alias Nostrum.Shard.Supervisor
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.User

  @doc """
  Gets the bot's OAuth2 application info.

  ## Example
  ```elixir
  Nostrum.Api.Self.application_information
  {:ok,
  %{
    bot_public: false,
    bot_require_code_grant: false,
    description: "Test",
    icon: nil,
    id: "172150183260323840",
    name: "Baba O-Riley",
    owner: %{
      avatar: nil,
      discriminator: "0042",
      id: "172150183260323840",
      username: "i own a bot"
    },
  }}
  ```
  """
  @spec application_information() :: Api.error() | {:ok, map()}
  def application_information do
    Api.request(:get, Constants.application_information())
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Gets info on the current user.

  If nostrum's caching is enabled, it is recommended to use `Me.get/0`
  instead of this function. This is because sending out an API request is much slower
  than pulling from our cache.

  If the request is successful, this function returns `{:ok, user}`, where
  `user` is nostrum's `Nostrum.Struct.User`. Otherwise, returns `{:error, reason}`.
  """
  @spec get() :: Api.error() | {:ok, User.t()}
  def get do
    Api.request(:get, Constants.me())
    |> Helpers.handle_request_with_decode({:struct, User})
  end

  @doc """
  Gets a list of our user's DM channels.

  If successful, returns `{:ok, dm_channels}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Examples

  ```elixir
  Nostrum.Api.Self.dms()
  {:ok, [%Nostrum.Struct.Channel{type: 1} | _]}
  ```
  """
  @spec dms() :: Api.error() | {:ok, [Channel.dm_channel()]}
  def dms do
    Api.request(:get, Constants.me_channels())
    |> Helpers.handle_request_with_decode({:list, {:struct, Channel}})
  end

  @doc ~S"""
  Changes the username or avatar of the current user.

  ## Options

    * `:username` (string) - new username
    * `:avatar` (string) - the user's avatar as [avatar data](https://discord.com/developers/docs/resources/user#avatar-data)

  ## Examples

  ```elixir
  Nostrum.Api.Self.modify(avatar: "data:image/jpeg;base64,YXl5IGJieSB1IGx1a2luIDQgc3VtIGZ1az8=")
  ```
  """
  @spec modify(Api.options()) :: Api.error() | {:ok, User.t()}
  def modify(options)

  def modify(options) when is_list(options),
    do: modify(Map.new(options))

  def modify(options) when is_map(options) do
    Api.request(:patch, Constants.me(), options)
    |> Helpers.handle_request_with_decode({:struct, User})
  end

  @doc """
  Updates the status of the bot for a certain shard.

  ## Parameters
    - `pid` - Pid of the shard.
    - `status` - Status of the bot.
    - `game` - The 'playing' text of the bot. Empty will clear.
    - `type` - The type of status to show. 0 (Playing) | 1 (Streaming) | 2 (Listening) | 3 (Watching)
    - `stream` - URL of twitch.tv stream
  """
  @spec update_shard_status(pid, Api.status(), String.t(), integer, String.t() | nil) :: :ok
  def update_shard_status(pid, status, game, type \\ 0, stream \\ nil) do
    Session.update_status(pid, to_string(status), game, stream, type)
    :ok
  end

  @doc """
  Updates the status of the bot for all shards.

  See `update_shard_status/5` for usage.
  """
  @spec update_status(Api.status(), String.t(), integer, String.t() | nil) :: :ok
  def update_status(status, game, type \\ 0, stream \\ nil) do
    _result = Supervisor.update_status(to_string(status), game, stream, type)
    :ok
  end

  @doc """
  Joins, moves, or disconnects the bot from a voice channel.

  The correct shard to send the update to will be inferred from the
  `guild_id`. If a corresponding `guild_id` is not found a cache error will be
  raised.

  To disconnect from a channel, `channel_id` should be set to `nil`.
  """
  @spec update_voice_state(Guild.id(), Channel.id() | nil, boolean, boolean) :: no_return | :ok
  def update_voice_state(guild_id, channel_id, self_mute \\ false, self_deaf \\ false) do
    Supervisor.update_voice_state(guild_id, channel_id, self_mute, self_deaf)
  end

  @doc """
  Gets a list of guilds the user is currently in.

  This endpoint requires the `guilds` OAuth2 scope.

  If successful, returns `{:ok, guilds}`. Otherwise, returns a `t:Nostrum.Api.error/0`.

  ## Options

    * `:before` (`t:Nostrum.Snowflake.t/0`) - get guilds before this
    guild ID
    * `:after` (`t:Nostrum.Snowflake.t/0`) - get guilds after this guild
    ID
    * `:limit` (integer) - max number of guilds to return (1-100)

  ## Examples

  ```elixir
  iex> Nostrum.Api.Self.guilds(limit: 1)
  {:ok, [%Nostrum.Struct.Guild{}]}
  ```
  """
  @spec guilds(Api.options()) :: Api.error() | {:ok, [Guild.user_guild()]}
  def guilds(options \\ [])

  def guilds(options) when is_list(options),
    do: guilds(Map.new(options))

  def guilds(options) when is_map(options) do
    Api.request(:get, Constants.me_guilds(), "", options)
    |> Helpers.handle_request_with_decode({:list, {:struct, Guild}})
  end

  @doc """
  Gets a list of user connections.
  """
  @spec connections() :: Api.error() | {:ok, list()}
  def connections do
    Api.request(:get, Constants.me_connections())
    |> Helpers.handle_request_with_decode()
  end
end
