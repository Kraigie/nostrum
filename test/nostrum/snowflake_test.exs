defmodule Nostrum.SnowflakeTest do
  use ExUnit.Case, async: true

  alias Nostrum.Snowflake

  require Snowflake

  doctest Snowflake

  describe "creation_time/1" do
    test "cannot raise error" do
      min_datetime = Snowflake.creation_time(0)
      max_datetime = Snowflake.creation_time(0xFFFFFFFFFFFFFFFF)

      assert(inspect(min_datetime) === "~U[2015-01-01 00:00:00.000Z]")
      assert(inspect(max_datetime) === "~U[2154-05-15 07:35:11.103Z]")
    end
  end
end
