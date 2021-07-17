defmodule Nostrum.Application do
  @moduledoc false

  alias Nostrum.Cache.GuildCache
  use Application

  require Logger

  @doc false
  def start(_type, _args) do
    check_token()
    check_executables()
    setup_ets_tables()

    children = [
      Nostrum.Api.Ratelimiter,
      Nostrum.Shard.Connector,
      Nostrum.Cache.CacheSupervisor,
      Nostrum.Shard.Supervisor,
      Nostrum.Voice.Supervisor
    ]

    if Application.get_env(:nostrum, :dev),
      do: Supervisor.start_link(children ++ [DummySupervisor], strategy: :one_for_one),
      else: Supervisor.start_link(children, strategy: :one_for_one)
  end

  @doc false
  def setup_ets_tables do
    :ets.new(:gateway_url, [:set, :public, :named_table])
    :ets.new(:unavailable_guilds, [:set, :public, :named_table])
    :ets.new(:users, [:set, :public, :named_table])
    :ets.new(:channels, [:set, :public, :named_table])
    :ets.new(:presences, [:set, :public, :named_table])
    :ets.new(:guild_shard_map, [:set, :public, :named_table])
    :ets.new(:channel_guild_map, [:set, :public, :named_table])
    :ets.new(GuildCache.tabname(), [:set, :public, :named_table])
  end

  defp check_token, do: check_token(Application.get_env(:nostrum, :token))
  defp check_token(nil), do: raise("Please supply a token")
  defp check_token(<<_::192, 46, _::48, 46, _::216>>), do: :ok

  defp check_token(_invalid_format),
    do: raise("Invalid token format, copy it again from the `Bot` tab of your Application")

  defp check_executables do
    ff = Application.get_env(:nostrum, :ffmpeg)
    yt = Application.get_env(:nostrum, :youtubedl)

    cond do
      is_binary(ff) and is_nil(System.find_executable(ff)) ->
        Logger.warn("""
        #{ff} was not found in your path. By default, Nostrum requires ffmpeg to use voice.
        If you don't intend to use voice with ffmpeg, configure :nostrum, :ffmpeg to nil to suppress.
        """)

      is_binary(yt) and is_nil(System.find_executable(yt)) ->
        Logger.warn("""
        #{yt} was not found in your path. Nostrum supports youtube-dl for voice.
        If you don't require youtube-dl support, configure :nostrum, :youtubedl to nil to suppress.
        """)

      true ->
        :ok
    end
  end
end
