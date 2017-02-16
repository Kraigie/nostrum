defmodule RatelimitTest do
  use ExUnit.Case, async: false

  @test_channel 179679229036724225
  @test_guild 179679229036724225

  setup do
    Mixcord.Api.Ratelimiter.empty_buckets
    :ok
  end

  @tag disabled: true
  test "one route sync no 429" do
    result = Enum.map(1..10, fn x -> Mixcord.Api.create_message(@test_channel, "#{x}") end)
    assert Enum.all?(result, fn x -> elem(x, 0) == :ok end) == true
  end

  @tag disabled: true
  test "one route async no 429" do
    responses =
      1..10
      |> Task.async_stream(&Mixcord.Api.create_message(@test_channel, "#{&1}"), timeout: 15000)
      |> Enum.to_list()
    assert Enum.all?(responses, fn x -> elem(x, 0) == :ok end) == true
  end

  _ = """
  Elixir streams are limited by cores, so the fifth batch of requests will be
  blocked, see:
  https://github.com/elixir-lang/elixir/blob/ac9637d0fbf8c40c6c84415f70f8ed3aa2351f4f/lib/elixir/lib/task/supervised.ex#L147

  The fifth batch will be blocked until the first batch that when through finishes.
  So, we should expect to see the 5th guild after the 4th pinned message, and
  this is the case.
  """
  test "multi route async no 429" do
    responses =
      1..5 #
      |> Task.async_stream(
        fn x ->
            # This guy should be peachy
          with {:ok, _ } <- Mixcord.Api.get_guild(@test_guild),
            IO.inspect("After guild #{x}"),
            # These should block each other after the second iteration
            {:ok, _} <- Mixcord.Api.create_message(@test_channel, "#{x}"),
            IO.inspect("After message create #{x}"),
            {:ok} <- Mixcord.Api.start_typing(@test_channel),
            IO.inspect("After typing start #{x}"),
            {:ok, _} <- Mixcord.Api.get_pinned_messages(@test_channel),
            IO.inspect("After get pinned #{x}")
          do
            :ok
          else
            f -> {:not_ok}
          end
        end,
        timeout: 20000)
      |> Enum.to_list()
    assert Enum.all?(responses, fn {k, v} -> v == :ok end) == true
  end
end
