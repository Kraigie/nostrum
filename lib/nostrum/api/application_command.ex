defmodule Nostrum.Api.ApplicationCommand do
  @moduledoc """
  Module for interacting with Discord's application commands.

  See: https://discord.com/developers/docs/interactions/application-commands
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Cache.Me
  alias Nostrum.Constants
  alias Nostrum.Snowflake
  alias Nostrum.Struct.ApplicationCommand
  alias Nostrum.Struct.Guild
  alias Nostrum.Struct.User

  @doc """
  Edits command permissions for a specific command for your application in a guild. You can only add up to 10 permission overwrites for a command.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.
  - `command_id`: Command ID to fetch permissions for.
  - `permissions`: List of partial [guild application command permissions](hhttps://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure) with `id` and `permissions`. You can add up to 10 overwrites per command.

  ## Return value
  This method returns a guild application command permission object, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "1.x.x"
  @spec batch_edit_permissions(Guild.id(), [
          %{
            id: Snowflake.t(),
            permissions: [ApplicationCommand.application_command_permissions()]
          }
        ]) ::
          {:ok, map()} | Api.error()
  @spec batch_edit_permissions(User.id(), Guild.id(), [
          %{
            id: Snowflake.t(),
            permissions: [ApplicationCommand.application_command_permissions()]
          }
        ]) ::
          {:ok, map()} | Api.error()
  def batch_edit_permissions(
        application_id \\ Me.get().id,
        guild_id,
        permissions
      ) do
    Api.request(
      :put,
      Constants.guild_application_command_permissions(application_id, guild_id),
      permissions
    )
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Overwrite the existing global application commands.

  This action will:
  - Create any command that was provided and did not already exist
  - Update any command that was provided and already existed if its configuration changed
  - Delete any command that was not provided but existed on Discord's end

  Updates will be available in all guilds after 1 hour.
  Commands that do not already exist will count toward daily application command create limits.

  ## Parameters
  - `application_id`: Application ID for which to overwrite the commands.
    If not given, this will be fetched from `Me`.
  - `commands`: List of command configurations, see the linked API documentation for reference.

  ## Return value
  Updated list of global application commands. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#bulk-overwrite-global-application-commands
  """
  @doc since: "1.x.x"
  @spec bulk_overwrite_global_commands([
          ApplicationCommand.application_command_map()
        ]) ::
          {:ok, [map()]} | Api.error()
  @spec bulk_overwrite_global_commands(User.id(), [
          ApplicationCommand.application_command_map()
        ]) :: {:ok, [map()]} | Api.error()
  def bulk_overwrite_global_commands(application_id \\ Me.get().id, commands) do
    Api.request(:put, Constants.global_application_commands(application_id), commands)
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Overwrite the existing guild application commands on the specified guild.

  This action will:
  - Create any command that was provided and did not already exist
  - Update any command that was provided and already existed if its configuration changed
  - Delete any command that was not provided but existed on Discord's end

  ## Parameters
  - `application_id`: Application ID for which to overwrite the commands.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild on which to overwrite the commands.
  - `commands`: List of command configurations, see the linked API documentation for reference.

  ## Return value
  Updated list of guild application commands. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#bulk-overwrite-guild-application-commands
  """
  @doc since: "1.x.x"
  @spec bulk_overwrite_guild_commands(Guild.id(), [
          ApplicationCommand.application_command_map()
        ]) :: {:ok, [map()]} | Api.error()
  @spec bulk_overwrite_guild_commands(User.id(), Guild.id(), [
          ApplicationCommand.application_command_map()
        ]) ::
          {:ok, [map()]} | Api.error()
  def bulk_overwrite_guild_commands(
        application_id \\ Me.get().id,
        guild_id,
        commands
      ) do
    Api.request(:put, Constants.guild_application_commands(application_id, guild_id), commands)
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Create a new global application command.

  The new command will be available on all guilds in around an hour.
  If you want to test commands, use `create_guild_command/2` instead,
  as commands will become available instantly there.
  If an existing command with the same name exists, it will be overwritten.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The created command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#create-global-application-command

  ## Example

  ```elixir
  Nostrum.Api.ApplicationCommand.create_global_command(
    %{name: "edit", description: "ed, man! man, ed", options: []}
  )
  ```
  """
  @spec create_global_command(ApplicationCommand.application_command_map()) ::
          {:ok, map()} | Api.error()
  @spec create_global_command(
          User.id(),
          ApplicationCommand.application_command_map()
        ) ::
          {:ok, map()} | Api.error()
  def create_global_command(application_id \\ Me.get().id, command) do
    Api.request(:post, Constants.global_application_commands(application_id), command)
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Create a guild application command on the specified guild.

  The new command will be available immediately.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild on which to create the command.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The created command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#create-guild-application-command
  """
  @spec create_guild_command(Guild.id(), ApplicationCommand.application_command_map()) ::
          {:ok, map()} | Api.error()
  @spec create_guild_command(
          User.id(),
          Guild.id(),
          ApplicationCommand.application_command_map()
        ) :: {:ok, map()} | Api.error()
  def create_guild_command(
        application_id \\ Me.get().id,
        guild_id,
        command
      ) do
    Api.request(:post, Constants.guild_application_commands(application_id, guild_id), command)
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Delete an existing global application command.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `command_id`: The current snowflake of the command.
  """
  @spec delete_global_command(Snowflake.t()) :: {:ok} | Api.error()
  @spec delete_global_command(User.id(), Snowflake.t()) :: {:ok} | Api.error()
  def delete_global_command(application_id \\ Me.get().id, command_id) do
    Api.request(:delete, Constants.global_application_command(application_id, command_id))
  end

  @doc """
  Delete an existing guild application command.

  ## Parameters
  - `application_id`: Application ID for which to create the command.
    If not given, this will be fetched from `Me`.
  - `guild_id`: The guild on which the command exists.
  - `command_id`: The current snowflake of the command.
  """
  @spec delete_guild_command(Guild.id(), Snowflake.t()) :: {:ok} | Api.error()
  @spec delete_guild_command(User.id(), Guild.id(), Snowflake.t()) ::
          {:ok} | Api.error()
  def delete_guild_command(
        application_id \\ Me.get().id,
        guild_id,
        command_id
      ) do
    Api.request(
      :delete,
      Constants.guild_application_command(application_id, guild_id, command_id)
    )
  end

  @doc """
  Edits command permissions for a specific command for your application in a guild. You can only add up to 10 permission overwrites for a command.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.
  - `command_id`: Command ID to fetch permissions for.
  - `permissions`: List of [application command permissions](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-application-command-permissions-structure)

  ## Return value
  This method returns a guild application command permission object, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "1.x.x"
  @spec edit_command_permissions(Guild.id(), Snowflake.t(), [
          ApplicationCommand.application_command_permissions()
        ]) ::
          {:ok, map()} | Api.error()
  @spec edit_command_permissions(User.id(), Guild.id(), Snowflake.t(), [
          ApplicationCommand.application_command_permissions()
        ]) ::
          {:ok, map()} | Api.error()
  def edit_command_permissions(
        application_id \\ Me.get().id,
        guild_id,
        command_id,
        permissions
      ) do
    Api.request(
      :put,
      Constants.guild_application_command_permissions(application_id, guild_id, command_id),
      %{
        permissions: permissions
      }
    )
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Update an existing global application command.

  The updated command will be available on all guilds in around an hour.

  ## Parameters
  - `application_id`: Application ID for which to edit the command.
    If not given, this will be fetched from `Me`.
  - `command_id`: The current snowflake of the command.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The updated command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#edit-global-application-command
  """
  @spec edit_global_command(
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) :: {:ok, map()} | Api.error()
  @spec edit_global_command(
          User.id(),
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) :: {:ok, map()} | Api.error()
  def edit_global_command(
        application_id \\ Me.get().id,
        command_id,
        command
      ) do
    Api.request(:patch, Constants.global_application_command(application_id, command_id), command)
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Update an existing guild application command.

  The updated command will be available immediately.

  ## Parameters
  - `application_id`: Application ID for which to edit the command.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild for which to update the command.
  - `command_id`: The current snowflake of the command.
  - `command`: Command configuration, see the linked API documentation for reference.

  ## Return value
  The updated command. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#edit-guild-application-command
  """
  @spec edit_guild_command(
          Guild.id(),
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) :: {:ok, map()} | Api.error()
  @spec edit_guild_command(
          User.id(),
          Guild.id(),
          Snowflake.t(),
          ApplicationCommand.application_command_edit_map()
        ) ::
          {:ok, map()} | Api.error()
  def edit_guild_command(
        application_id \\ Me.get().id,
        guild_id,
        command_id,
        command
      ) do
    Api.request(
      :patch,
      Constants.guild_application_command(application_id, guild_id, command_id),
      command
    )
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Fetches command permissions for a specific command for your application in a guild.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.
  - `command_id`: Command ID to fetch permissions for.

  ## Return value
  This method returns a single guild application command permission object, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "1.x.x"
  @spec permissions(Guild.id(), Snowflake.t()) ::
          {:ok, map()} | Api.error()
  @spec permissions(User.id(), Guild.id(), Snowflake.t()) ::
          {:ok, map()} | Api.error()
  def permissions(
        application_id \\ Me.get().id,
        guild_id,
        command_id
      ) do
    Api.request(
      :get,
      Constants.guild_application_command_permissions(application_id, guild_id, command_id)
    )
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Fetch all global commands.

  ## Parameters
  - `application_id`: Application ID for which to search commands.
    If not given, this will be fetched from `Me`.

  ## Return value
  A list of ``ApplicationCommand``s on success. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-structure

  ## Example

  ```elixir
  iex> Nostrum.Api.global_commands
  {:ok,
   [
     %{
       application_id: "455589479713865749",
       description: "ed, man! man, ed",
       id: "789841753196331029",
       name: "edit"
     }
   ]}
  ```
  """
  @spec global_commands() :: {:ok, [map()]} | Api.error()
  @spec global_commands(User.id()) :: {:ok, [map()]} | Api.error()
  def global_commands(application_id \\ Me.get().id) do
    Api.request(:get, Constants.global_application_commands(application_id))
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Fetches command permissions for all commands for your application in a guild.

  ## Parameters
  - `application_id`: Application ID commands are registered under.
    If not given, this will be fetched from `Me`.
  - `guild_id`: Guild ID to fetch command permissions from.

  ## Return value
  This method returns a list of guild application command permission objects, see all available values on the [Discord API docs](https://discord.com/developers/docs/interactions/application-commands#application-command-permissions-object-guild-application-command-permissions-structure).
  """
  @doc since: "1.x.x"
  @spec guild_permissions(Guild.id()) :: {:ok, [map()]} | Api.error()
  @spec guild_permissions(User.id(), Guild.id()) ::
          {:ok, [map()]} | Api.error()
  def guild_permissions(
        application_id \\ Me.get().id,
        guild_id
      ) do
    Api.request(:get, Constants.guild_application_command_permissions(application_id, guild_id))
    |> Helpers.handle_request_with_decode()
  end

  @doc """
  Fetch all guild application commands for the given guild.

  ## Parameters
  - `application_id`: Application ID for which to fetch commands.
    If not given, this will be fetched from `Me`.
  - `guild_id`: The guild ID for which guild application commands
    should be requested.

  ## Return value
  A list of ``ApplicationCommand``s on success. See the official reference:
  https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-structure
  """
  @spec guild_commands(Guild.id()) :: {:ok, [map()]} | Api.error()
  @spec guild_commands(User.id(), Guild.id()) :: {:ok, [map()]} | Api.error()
  def guild_commands(application_id \\ Me.get().id, guild_id) do
    Api.request(:get, Constants.guild_application_commands(application_id, guild_id))
    |> Helpers.handle_request_with_decode()
  end
end
