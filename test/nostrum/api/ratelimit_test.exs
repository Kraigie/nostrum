defmodule Nostrum.Api.RatelimitTest do
  use ExUnit.Case, async: true

  @test_channel 179_679_229_036_724_225
  @test_guild 179_679_229_036_724_225
  @test_message 424_789_811_463_847_941
  @test_user 89_918_932_789_497_856
  @test_users [89_918_932_789_497_856, 259_856_429_450_526_720]

  setup do
    :ok
  end

  test "endpoint with major parameter" do
    expected = "/channels/#{@test_channel}/messages/_id"

    result =
      Nostrum.Api.Ratelimiter.get_endpoint(
        "/channels/#{@test_channel}/messages/#{@test_message}",
        :get
      )

    assert result == expected
  end

  test "message delete endpoint" do
    expected = "delete:/channels/#{@test_channel}/messages/_id"

    result =
      Nostrum.Api.Ratelimiter.get_endpoint(
        "/channels/#{@test_channel}/messages/#{@test_message}",
        :delete
      )

    assert result == expected
  end

  test "endpoint with no major parameter" do
    expected = "/users/_id"

    result = Nostrum.Api.Ratelimiter.get_endpoint("/users/#{@test_user}", :get)

    assert result == expected
  end

  test "reaction endpoint" do
    expected = "/channels/#{@test_channel}/messages/_id/reactions"

    result =
      Nostrum.Api.Ratelimiter.get_endpoint(
        "/channels/#{@test_channel}/messages/#{@test_message}/reactions/⬅/@me",
        :get
      )

    assert result == expected
  end

  @tag disabled: true
  test "non-major parameter async no 429" do
    [first, second] = @test_users

    responses =
      1..2
      |> Task.async_stream(
        fn _ ->
          with {:ok, _} <- Nostrum.Api.get_user(first),
               {:ok, _} <- Nostrum.Api.get_user(second) do
            :ok
          else
            o ->
              IO.inspect(o)
              {:not_ok}
          end
        end,
        timeout: 50000
      )
      |> Enum.to_list()

    assert Enum.all?(responses, fn {_k, v} -> v == :ok end) == true
  end

  @tag disabled: true
  test "one route sync no 429" do
    result = Enum.map(1..10, fn x -> Nostrum.Api.create_message(@test_channel, "#{x}") end)
    assert Enum.all?(result, fn x -> elem(x, 0) == :ok end) == true
  end

  @tag disabled: true
  test "one route async no 429" do
    responses =
      1..11
      |> Task.async_stream(&Nostrum.Api.create_message(@test_channel, "#{&1}"), timeout: 50000)
      |> Enum.to_list()

    assert Enum.all?(responses, fn {_, {k, _}} -> k == :ok end) == true
  end

  @tag disabled: true
  test "one route async no 429 pins" do
    responses =
      1..5
      |> Task.async_stream(
        fn _ -> Nostrum.Api.get_pinned_messages(@test_channel) end,
        timeout: 50000
      )
      |> Enum.to_list()

    assert Enum.all?(responses, fn {_, {k, _}} -> k == :ok end) == true
  end

  _ = """
  Elixir streams are limited by cores, so the fifth batch of requests will be
  blocked, see:
  https://github.com/elixir-lang/elixir/blob/ac9637d0fbf8c40c6c84415f70f8ed3aa2351f4f/lib/elixir/lib/task/supervised.ex#L147

  The fifth batch will be blocked until the first batch that when through finishes.
  So, we should expect to see the 5th guild after the 4th pinned message, and
  this is the case. We could set the concurrency manually, but it's a cool example.
  """

  @tag disabled: true
  test "multi route async no 429" do
    responses =
      1..10
      |> Task.async_stream(
        fn x ->
          with {:ok, _} <- Nostrum.Api.get_guild(@test_guild),
               {:ok, _} <- Nostrum.Api.create_message(@test_channel, "#{x}"),
               {:ok, _} <- Nostrum.Api.get_channel_message(@test_channel, @test_message),
               {:ok} <- Nostrum.Api.start_typing(@test_channel) do
            :ok
          else
            _ ->
              {:not_ok}
          end
        end,
        timeout: 50000
      )
      |> Enum.to_list()

    assert Enum.all?(responses, fn {_k, v} -> v == :ok end) == true
  end
end
