defmodule Nostrum.StateMachineTranslator do
  @moduledoc """
  Translate error reports for `:gen_statem` modules in Elixir.

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and people curious to look behind the covers.

  ## Reasoning

  By default, Elixir will ignore these messages altogether, see [this
  ElixirForum
  post](https://elixirforum.com/t/why-does-logger-translator-ignore-gen-statem-reports/37418).
  A possible workaround seems to be using the `gen_state_machine` library, but
  pulling in a library purely to have error reporting for something built-in to
  OTP seems pretty strange to me.
  """
  @moduledoc since: "0.9.0"

  # https://github.com/elixir-lang/elixir/blob/d30c5c0185607f08797441ab8af12636ad8dbd7e/lib/logger/lib/logger/translator.ex#L39
  def translate(min_level, :error, :report, {:logger, %{label: label} = report}) do
    case label do
      {:gen_statem, :terminate} ->
        report_gen_statem_terminate(min_level, report)

      _ ->
        :none
    end
  end

  def translate(_min_level, _level, _kind, _data) do
    :none
  end

  defp report_gen_statem_terminate(_min_level, report) do
    # raw erlang format. you know it, you love it.
    # thanks to OTP for exposing this in the first place.
    log = :gen_statem.format_log(report, %{})
    {:ok, :erlang.list_to_binary(log), []}
  end
end
