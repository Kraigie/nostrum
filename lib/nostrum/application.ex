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

    unless Application.get_env(:nostrum, :suppress_youtubedl_version) do
      if String.contains?(Application.get_env(:nostrum, :youtubedl, "youtube-dl"), "youtube-dl"),
        do: check_youtubedl()
    end

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

  # youtube-dl has not received active maintenance in the form of a new release
  # since 2021, as a result of this it no longer works with youtube (though it
  # DOES work with some other services)
  #
  # We opt here to give users the choice on what library to use, and allow for
  # suppresssion of this warning, but we advise that using the default
  # youtube-dl install may not work with YouTube.
  defp check_youtubedl do
    with bin when is_binary(bin) <-
           System.find_executable(Application.get_env(:nostrum, :youtubedl, "youtube-dl")),
         {version, 0} <- System.cmd(bin, ["--version"], stderr_to_stdout: true),
         {:ok, version_year} when version_year <= @outdated_youtubedl_version <-
           get_youtubedl_version_year(version) do
      Logger.warning("""
      Located youtube-dl installation at '#{bin}' is version #{version |> String.trim_trailing()}.

      This version is known to not support recent YouTube updates preventing it from being able to stream content.

      It is advised to move to a maintained fork such as yt-dlp and configure :nostrum, :youtubedl to point to this version.

      If you know what you are doing, configure :nostrum, :suppress_youtubedl_version to true to silence this warning
      """)
    end
  end

  # Helper to parse 2021 out of "2021.12.17"
  defp get_youtubedl_version_year(version) do
    with [year | _rest] <- String.split(version, "."),
         {year, ""} <- Integer.parse(year) do
      {:ok, year}
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
