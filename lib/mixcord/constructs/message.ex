defmodule Mixcord.Constructs.Message do
  @moduledoc """
  Struct representing a Discord message and various helper functions.
  """

  @derive [Poison.Encoder]
  defstruct [
    :attachments,
    :author,
    :channel_id,
    :content,
    :edited_timestamp,
    :embeds,
    :id,
    :mention_everyone,
    :mention_roles,
    :mentions,
    :nonce,
    :pinned,
    :timestamp,
    :tts,
    :type,
  ]

  def get_member do

  end
end