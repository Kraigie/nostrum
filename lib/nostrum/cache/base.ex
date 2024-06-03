defmodule Nostrum.Cache.Base do
  # Utilities for nostrum caches.
  @moduledoc since: "0.8.0"
  @moduledoc false

  @dialyzer {:nowarn_function, get_cache_module: 2, get_cache_options: 1}

  @guild_cache_config Application.compile_env(:nostrum, [:caches, :guilds])
  @channel_guild_mapping_config Application.compile_env(:nostrum, [
                                  :caches,
                                  :channel_guild_mapping
                                ])
  @member_cache_config Application.compile_env(:nostrum, [:caches, :members])
  @message_cache_config Application.compile_env(:nostrum, [:caches, :messages])
  @presence_cache_config Application.compile_env(:nostrum, [:caches, :presences])
  @user_cache_config Application.compile_env(:nostrum, [:caches, :users])

  @cache_map %{
    guilds: @guild_cache_config,
    channel_guild_mapping: @channel_guild_mapping_config,
    members: @member_cache_config,
    messages: @message_cache_config,
    presences: @presence_cache_config,
    users: @user_cache_config
  }

  def mnesia_note,
    do: """
    Please note that this module is only compiled if Mnesia is available on
    your system. See the Mnesia section of the [State](functionality/state.md)
    documentation for more information.

    To retrieve the table name used by this cache, use `table/0`.
    """

  @moduledoc since: "0.10.0"
  def get_cache_module(cache_name, default) do
    case @cache_map do
      %{^cache_name => nil} -> default
      %{^cache_name => {module, _opts}} when is_atom(module) -> module
      %{^cache_name => module} when is_atom(module) -> module
    end
  end

  @moduledoc since: "0.10.0"
  def get_cache_options(cache_name) do
    case @cache_map do
      %{^cache_name => nil} -> []
      %{^cache_name => {_, opts}} -> opts
      %{^cache_name => _} -> []
    end
  end
end
