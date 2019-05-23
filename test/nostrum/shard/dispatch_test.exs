defmodule Nostrum.Shard.DispatchTest do
  use ExUnit.Case, async: true

  alias Nostrum.Shard.Dispatch
  alias Nostrum.Struct.MessageDeleteEvent
  alias Nostrum.Stubs

  describe "handle_event/1" do
    test "returns `%MessageDeleteEvent{}` given Message Delete Event" do
      result = Dispatch.handle_event(:MESSAGE_DELETE, Stubs.gateway_message_delete_payload(), %{})

      expected =
        {:MESSAGE_DELETE,
         %MessageDeleteEvent{
           channel_id: 453_700_913_115_168_768,
           guild_id: 279_093_381_723_062_272,
           id: 580_112_111_657_549_834
         }, %{}}

      assert(^result = expected)
    end
  end
end
