defmodule Nostrum.Struct.SnowflakeTest do
  use ExUnit.Case

  alias Nostrum.Struct.Snowflake

  require Nostrum.Struct.Snowflake

  test "cast/1: if string is given, then return snowflake" do
    given = "4384931048190393"
    expected = {:ok, 4384931048190393}

    observed = Snowflake.cast(given)

    assert ^expected = observed
  end

  test "cast/1: if integer is given, then return snowflake" do
    given = 4384931048190393
    expected = {:ok, 4384931048190393}

    observed = Snowflake.cast(given)

    assert ^expected = observed
  end

  test "cast/1: if nil is given, then return nil" do
    given = nil
    expected = {:ok, nil}

    observed = Snowflake.cast(given)

    assert ^expected = observed
  end

  test "cast/1: if non-convertible is given, then return :error" do
    given = true
    expected = :error

    observed = Snowflake.cast(given)

    assert ^expected = observed
  end

  test "dump/1: if snowflake is given, then return snowflake as string" do
    given = 4314103984319043
    expected = "4314103984319043"

    observed = Snowflake.dump(given)

    assert ^expected = observed
  end

  test "dump/1: if non-snowflake is given, then raise exception" do
    given = :not_snowflake

    assert_raise(ArgumentError, fn ->
      Snowflake.dump(given)
    end)
  end

  test "is_snowflake/1: if snowflake is given, return true" do
    given = 4314831498137
    expected = true

    observed = Snowflake.is_snowflake(given)

    assert expected === observed
  end
  
  test "is_snowflake/1: if snowflake is given, return false" do
    given = "4314831498137"
    expected = false

    observed = Snowflake.is_snowflake(given)

    assert expected === observed
  end
end
