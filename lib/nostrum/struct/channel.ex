defmodule Nostrum.Struct.Channel do
  @moduledoc """
  Functions to help work with different channel types.
  """

  alias Nostrum.Struct.DMChannel
  alias Nostrum.Struct.Guild.{TextChannel, VoiceChannel}

  @type t :: DMChannel.t | TestChannel.t | VoiceChannel.t

  def to_struct(%{is_private: true} = map), do: DMChannel.to_struct(map)
  def to_struct(%{topic: _} = map), do: TextChannel.to_struct(map)
  def to_struct(%{bitrate: _} = map), do: VoiceChannel.to_struct(map)
end
