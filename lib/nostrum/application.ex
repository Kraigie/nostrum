defmodule Nostrum.Application do
  @moduledoc false

  use Application

  alias Nostrum.Token

  require Logger

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
      else: Supervisor.start_link(children, strategy: :one_for_one)
  end

  defp check_executables do
    ff = Application.get_env(:nostrum, :ffmpeg)
    yt = Application.get_env(:nostrum, :youtubedl)
    sl = Application.get_env(:nostrum, :streamlink)

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

      is_binary(sl) and is_nil(System.find_executable(sl)) ->
        Logger.warn("""
        #{sl} was not found in your path. Nostrum supports streamlink for voice.
        If you don't require streamlink support, configure :nostrum, :streamlink to nil to suppress.
        """)

      true ->
        :ok
    end
  end
end
