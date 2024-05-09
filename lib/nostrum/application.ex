defmodule Nostrum.Application do
  @moduledoc false

  use Application

  alias Nostrum.Token

  require Logger

  @outdated_youtubedl_version 2021

  # Used for starting nostrum when running as an included application.
  def child_spec(_opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start, [:normal, []]},
      type: :supervisor,
      restart: :permanent,
      shutdown: 500
    }
  end

  @doc false
  def start(_type, _args) do
    Token.check_token!()
    check_executables()
    check_otp_version()
    Logger.add_translator({Nostrum.StateMachineTranslator, :translate})

    children = [
      Nostrum.Store.Supervisor,
      Nostrum.ConsumerGroup,
      Nostrum.Api.Ratelimiter,
      Nostrum.Shard.Connector,
      Nostrum.Cache.CacheSupervisor,
      Nostrum.Shard.Supervisor,
      Nostrum.Voice.Supervisor
    ]

    if Application.get_env(:nostrum, :dev),
      do: Supervisor.start_link(children ++ [DummySupervisor], strategy: :one_for_one),
      else: Supervisor.start_link(children, strategy: :one_for_one, name: Nostrum.Supervisor)
  end

  defp check_executables do
    ff = Application.get_env(:nostrum, :ffmpeg)
    yt = Application.get_env(:nostrum, :youtubedl)
    sl = Application.get_env(:nostrum, :streamlink)

    cond do
      is_binary(ff) and is_nil(System.find_executable(ff)) ->
        Logger.warning("""
        #{ff} was not found in your path. By default, Nostrum requires ffmpeg to use voice.
        If you don't intend to use voice with ffmpeg, configure :nostrum, :ffmpeg to nil to suppress.
        """)

      is_binary(yt) and is_nil(System.find_executable(yt)) ->
        Logger.warning("""
        #{yt} was not found in your path. Nostrum supports youtube-dl for voice.
        If you don't require youtube-dl support, configure :nostrum, :youtubedl to nil to suppress.
        """)

      is_binary(sl) and is_nil(System.find_executable(sl)) ->
        Logger.warning("""
        #{sl} was not found in your path. Nostrum supports streamlink for voice.
        If you don't require streamlink support, configure :nostrum, :streamlink to nil to suppress.
        """)

      true ->
        :ok
    end
  end

  defp check_otp_version do
    _module_info = :pg.module_info()

    unless function_exported?(:pg, :monitor, 2) do
      Logger.critical("""
      Your Erlang/OTP version needs to be 25.1 or newer to use Nostrum 0.9 and newer.
      Current major version: #{System.otp_release()}
      """)
    end
  end
end
