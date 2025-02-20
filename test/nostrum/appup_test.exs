defmodule Nostrum.AppupTest do
  use ExUnit.Case, async: true

  @raw_version Nostrum.Mixfile.project()[:version]
  @version Version.parse!(@raw_version)

  describe "appup" do
    test "matches the current release" do
      path = Mix.Project.config()[:appup]
      {appup, []} = Code.eval_file(path)
      {raw_target, _up_instructions, _down_instructions} = appup
      target = Version.parse!(:erlang.list_to_binary(raw_target))

      assert target == @version
    end
  end
end
