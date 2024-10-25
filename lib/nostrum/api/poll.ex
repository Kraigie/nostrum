defmodule Nostrum.Api.Poll do
  alias Nostrum.Api
  alias Nostrum.Constants
  alias Nostrum.Util
  alias Nostrum.Struct.Message
  alias Nostrum.Struct.Channel
  alias Nostrum.Struct.Message.Poll.Answer
  alias Nostrum.Struct.User

  @doc ~S"""
  Expire (close voting on) a poll before the scheduled end time.

  Returns the original message containing the poll.
  """
  @spec expire(Channel.id(), Message.id()) :: Api.error() | {:ok, Message.t()}
  def expire(channel_id, message_id) do
    Api.request(:post, Constants.poll_expire(channel_id, message_id))
    |> Api.handle_request_with_decode({:struct, Message})
  end

  @doc ~S"""
  Get voters for the provided answer on the poll attached to the provided message.

  If successful, returns `{:ok, users}`. Otherwise, returns `t:Nostrum.Api.error/0`.

  The optional `params` are `after`, the user ID to query after, absent by default,
  and `limit`, the max number of users to return, 1-100, 25 by default. Results are
  sorted by Discord user snowflake (ID) in ascending order.
  """
  @spec answer_voters(Channel.id(), Message.id(), Answer.answer_id()) ::
          Api.error() | {:ok, [User.t()]}
  def answer_voters(channel_id, message_id, answer_id, params \\ []) do
    result =
      Api.request(
        :get,
        Constants.poll_answer_voters(channel_id, message_id, answer_id),
        "",
        params
      )
      |> Api.handle_request_with_decode()

    case result do
      {:ok, %{users: users}} -> {:ok, Util.cast(users, {:list, {:struct, User}})}
      _ -> result
    end
  end
end
