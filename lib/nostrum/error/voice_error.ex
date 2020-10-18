defmodule Nostrum.Error.VoiceError do
  @moduledoc """
  Represents an error when playing sound through voice channels.

  This occurs when attempting to play audio and Porcelain can't find either
  the ffmpeg executable or the youtube-dl executable.
  """

  defexception [:message]

  def exception(reason: reason, executable: executable) do
    msg = "ERROR: #{reason} - #{executable} improperly configured"
    %__MODULE__{message: msg}
  end
end
