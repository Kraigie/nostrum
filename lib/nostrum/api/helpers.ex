defmodule Nostrum.Api.Helpers do
  @moduledoc false

  alias Nostrum.Util

  defguard has_files(args) when is_map_key(args, :files) or is_map_key(args, :file)

  def handle_request_with_decode(response)
  def handle_request_with_decode({:ok, body}), do: {:ok, Jason.decode!(body, keys: :atoms)}
  def handle_request_with_decode({:error, _} = error), do: error

  def handle_request_with_decode(response, type)
  # add_guild_member/3 can return both a 201 and a 204
  def handle_request_with_decode({:ok}, _type), do: {:ok}
  def handle_request_with_decode({:error, _} = error, _type), do: error

  def handle_request_with_decode({:ok, body}, type) do
    convert =
      body
      |> Jason.decode!(keys: :atoms)
      |> Util.cast(type)

    {:ok, convert}
  end
end
