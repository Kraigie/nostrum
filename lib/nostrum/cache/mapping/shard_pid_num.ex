defmodule Nostrum.Cache.Mapping.ShardPidNum do
  @moduledoc """
  Simple mapping of shard pid to num
  """

  @doc """
  Gets the a shard pid from a shard number.
  """
  @spec get_pid(integer) :: no_return | pid
  def get_pid(shard_num) do
    :ets.lookup_element(:shard_pid_num, shard_num, 2)
  end

end
