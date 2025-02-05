defmodule Nostrum.Api.AutoModeration do
  @moduledoc """
  Functions for interacting with the Discord API's auto-moderation endpoints.

  See: https://discord.com/developers/docs/resources/auto-moderation
  """
  @moduledoc since: "0.10.1"
  alias Nostrum.Api
  alias Nostrum.Api.Helpers
  alias Nostrum.Constants
  alias Nostrum.Struct.AutoModerationRule
  alias Nostrum.Struct.Guild

  @doc """
  Create a new auto-moderation rule for a guild.

  ## Options
    * `:name` (`t:String.t/0`) - The name of the rule.
    * `:event_type` (`t:AutoModerationRule.event_type/0`) - The type of event that triggers the rule.
    * `:trigger_type` (`t:AutoModerationRule.trigger_type/0`) - The type of content that triggers the rule.
    * `:trigger_metadata` (`t:AutoModerationRule.trigger_metadata/0`) - The metadata associated with the rule trigger.
      - optional, based on the `:trigger_type`.
    * `:actions` (`t:AutoModerationRule.actions/0`) - The actions to take when the rule is triggered.
    * `:enabled` (`t:AutoModerationRule.enabled/0`) - Whether the rule is enabled or not.
      - optional, defaults to `false`.
    * `:exempt_roles` - (`t:AutoModerationRule.exempt_roles/0`) - A list of role id's that are exempt from the rule.
      - optional, defaults to `[]`, maximum of 20.
    * `:exempt_channels` - (`t:AutoModerationRule.exempt_channels/0`) - A list of channel id's that are exempt from the rule.
      - optional, defaults to `[]`, maximum of 50.
  """
  @doc since: "1.x.x"
  @spec create_rule(Guild.id(), Api.options()) ::
          {:ok, AutoModerationRule.t()} | Api.error()
  def create_rule(guild_id, options) when is_list(options),
    do: create_rule(guild_id, Map.new(options))

  def create_rule(guild_id, options) do
    Api.request(:post, Constants.guild_auto_moderation_rule(guild_id), options)
    |> Helpers.handle_request_with_decode({:struct, AutoModerationRule})
  end

  @doc """
  Delete an auto-moderation rule for a guild.
  """
  @doc since: "1.x.x"
  @spec delete_rule(Guild.id(), AutoModerationRule.id()) ::
          {:ok} | Api.error()
  def delete_rule(guild_id, rule_id) do
    Api.request(:delete, Constants.guild_auto_moderation_rule(guild_id, rule_id))
  end

  @doc """
  Get a list of all auto-moderation rules for a guild.
  """
  @doc since: "1.x.x"
  @spec rules(Guild.id()) :: {:ok, [AutoModerationRule.t()]} | Api.error()
  def rules(guild_id) do
    Api.request(:get, Constants.guild_auto_moderation_rule(guild_id))
    |> Helpers.handle_request_with_decode({:list, {:struct, AutoModerationRule}})
  end

  @doc """
  Get a single auto-moderation rule for a guild.
  """
  @doc since: "1.x.x"
  @spec rule(Guild.id(), AutoModerationRule.id()) ::
          {:ok, AutoModerationRule.t()} | Api.error()
  def rule(guild_id, rule_id) do
    Api.request(:get, Constants.guild_auto_moderation_rule(guild_id, rule_id))
    |> Helpers.handle_request_with_decode({:struct, AutoModerationRule})
  end

  @doc """
  Modify an auto-moderation rule for a guild.

  Takes the same options as `create_rule/2`, however all fields are optional.
  """
  @doc since: "1.x.x"
  @spec modify_rule(Guild.id(), AutoModerationRule.id(), Api.options()) ::
          {:ok, AutoModerationRule.t()} | Api.error()
  def modify_rule(guild_id, rule_id, options) when is_list(options),
    do: modify_rule(guild_id, rule_id, Map.new(options))

  def modify_rule(guild_id, rule_id, options) do
    Api.request(:patch, Constants.guild_auto_moderation_rule(guild_id, rule_id), options)
    |> Helpers.handle_request_with_decode({:struct, AutoModerationRule})
  end
end
