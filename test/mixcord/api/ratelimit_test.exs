defmodule RatelimitTest do
  use ExUnit.Case, async: false

  @test_channel 179679229036724225

  test "one route no 429 sync" do
    result = Enum.map(1..10, fn x -> Mixcord.Api.create_message!(@test_channel, "#{x}") end)
    assert Enum.all?(result, fn x -> elem(x, 0) == :ok end) == true
  end

  test "one route no 429 async" do
    result = Enum.map(1..10, fn x -> Task.start(fn -> Mixcord.Api.create_message!(@test_channel, "#{x}") end) end)
    assert Enum.all?(result, fn x -> elem(x, 0) == :ok end) == true
  end

  test "multi route no 429 sync" do

  end

  test "multi route no 429 async" do

  end
end