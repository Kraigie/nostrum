defmodule Nostrum.Struct.SnowflakeTest do
  use ExUnit.Case, async: true

  alias Nostrum.Struct.Snowflake

  require Snowflake

  doctest Snowflake
end
