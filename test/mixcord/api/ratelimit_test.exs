defmodule RatelimitTest do
  use ExUnit.Case, async: false

  @test_channel 179679229036724225

  test "one route sync no 429" do
    result = Enum.map(1..10, fn x -> Mixcord.Api.create_message!(@test_channel, "#{x}") end)
    assert Enum.all?(result, fn x -> elem(x, 0) == :ok end) == true
  end

  test "one route async no 429" do
    responses =
      1..10
      |> Task.async_stream(&Mixcord.Api.create_message!(@test_channel, "#{&1}"))
      |> Enum.to_list()

    assert Enum.all?(responses, fn x -> elem(x, 0) == :ok end) == true
  end

  test "multi route sync no 429" do
    
  end

  test "multi route async no 429" do

  end
end