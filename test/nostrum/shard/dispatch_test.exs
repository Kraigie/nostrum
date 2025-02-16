defmodule Nostrum.Shard.DispatchTest do
  use ExUnit.Case, async: true

  alias Nostrum.Shard.Dispatch
  alias Nostrum.Struct.Event.{MessageDelete, MessageDeleteBulk}
  alias NostrumTest.Stubs

  describe "handle_event/1" do
    test "returns `MessageDelete.t()` given Message Delete event" do
      result = Dispatch.handle_event(:MESSAGE_DELETE, Stubs.gateway_message_delete_payload(), %{})

      expected =
        {:MESSAGE_DELETE,
         %MessageDelete{
           channel_id: 453_700_913_115_168_768,
           guild_id: 279_093_381_723_062_272,
           id: 580_112_111_657_549_834
         }, %{}}

      assert(^result = expected)
    end

    test "returns `MessageDeleteBulk.t()` given Message Delete Bulk event" do
      payload = Stubs.gateway_message_delete_bulk_payload()

      {key, event, _} = Dispatch.handle_event(:MESSAGE_DELETE_BULK, payload, %{})

      assert(^key = :MESSAGE_DELETE_BULK)
      assert(^event = struct(MessageDeleteBulk, Map.put(payload, :deleted_messages, [])))
    end
  end
end
