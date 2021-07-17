defmodule Stubs do
  @moduledoc false

  def gateway_message_delete_payload,
    do: %{
      "channel_id" => 453_700_913_115_168_768,
      :guild_id => 279_093_381_723_062_272,
      "id" => 580_112_111_657_549_834
    }

  def gateway_message_delete_bulk_payload,
    do: %{
      "channel_id" => 453_700_913_115_168_768,
      :guild_id => 279_093_381_723_062_272,
      "ids" => [279_093_381_723_062_272, 279_093_381_723_062_273]
    }
end
