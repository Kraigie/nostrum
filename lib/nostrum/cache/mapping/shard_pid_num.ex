defmodule Nostrum.Cache.Mapping.ShardPidNum do
  @moduledoc """
  Simple mapping of shard pid to num
  """

  @doc """
  Gets the a shard pid from a shard number.
  """
  @spec get_pid(integer) :: no_return | pid
  def get_pid(shard_num) do
    case :ets.lookup(:shard_pid_num, shard_num) do
      [{_shard_num, shard_pid}] -> {:ok, shard_pid}
      [] -> {:error, :id_not_found}
    end
  end
end
