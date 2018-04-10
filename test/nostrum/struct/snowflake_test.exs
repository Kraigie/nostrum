defmodule Nostrum.Struct.SnowflakeTest do
  use ExUnit.Case

  alias Nostrum.Struct.Snowflake

  require Snowflake

  describe "Snowflake.cast/1" do
    test "if string is given, then return snowflake" do
      expected = {:ok, 4_384_931_048_190_393}
      observed = Snowflake.cast("4384931048190393")

      assert observed === expected
    end

    test "if integer is given, then return snowflake" do
      expected = {:ok, 4_384_931_048_190_393}
      observed = Snowflake.cast(4_384_931_048_190_393)

      assert observed === expected
    end

    test "if nil is given, then return nil" do
      observed = Snowflake.cast(nil)
      expected = {:ok, nil}

      assert observed === expected
    end

    test "if non-convertible is given, then return :error" do
      observed = Snowflake.cast(true)
      expected = :error

      assert observed === expected
    end
  end

  describe "Snowflake.dump/1" do
    test "if snowflake is given, then return snowflake as string" do
      observed = Snowflake.dump(4_314_103_984_319_043)
      expected = "4314103984319043"

      assert observed === expected
    end

    test "if non-snowflake is given, then raise exception" do
      assert_raise(ArgumentError, fn ->
        Snowflake.dump(:not_snowflake)
      end)
    end
  end

  describe "Snowflake.is_snowflake/1" do
    test "if snowflake is given, return true" do
      expected = true
      observed = Snowflake.is_snowflake(4_314_831_498_137)

      assert expected === observed
    end

    test "if snowflake is given, return false" do
      expected = false
      observed = Snowflake.is_snowflake("4314831498137")

      assert expected === observed
    end
  end

  describe "Snowflake.creation_time/1" do
    test "if snowflake is given, return the correct timestamp" do
      {:ok, expected, _} = DateTime.from_iso8601("2016-05-05T21:04:13.203Z")
      observed = Snowflake.creation_time(177_888_205_536_886_784)

      assert expected === observed
    end
  end
end
