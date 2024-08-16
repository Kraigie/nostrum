defmodule Nostrum.Api.RatelimiterGroup do
  @moduledoc """
  Tracks ratelimiters and determines correct ratelimiters to use per request.

  ## Purpose

  In a multi-node setup, users want to be able to make API requests from any
  node in the cluster without having to worry about hitting ratelimits. This
  module serves as the mediator between API clients on any nodes and their
  target ratelimiter.

  > ### Internal module {: .info}
  >
  > This module is intended for exclusive usage inside of nostrum, and is
  > documented for completeness and people curious to look behind the covers.

  ## Approach

  A naive implementation might simply forward requests to the locally (on the
  same node) running ratelimiter. However, this falls short when modules on
  other nodes want to make API requests, as they then effectively begin
  tracking their own ratelimit state, rendering it inconsistent.

  Instead, the approach is that we have a locally running ratelimiter on each
  node, all of which are registered via the `:pg` process group managed by this
  module. When an API request comes in, we determine its ratelimit bucket (see
  `Nostrum.Api.Ratelimiter.get_endpoint/2`) and based on that, determine the
  target ratelimiter by selecting it from the list of known ratelimiters via
  `:erlang.phash2/2`.
  """

  @scope_name __MODULE__
  @group_name :ratelimiters

  @doc """
  Return a ratelimiter PID to use for requests to the given ratelimiter `bucket`.
  """
  @spec limiter_for_bucket(String.t()) :: pid()
  def limiter_for_bucket(bucket) do
    limiters = :pg.get_members(@scope_name, @group_name)
    # "Processes are returned in no specific order."
    sorted = Enum.sort(limiters)
    total = length(sorted)
    selected = :erlang.phash2(bucket, total)
    Enum.at(sorted, selected)
  end

  @doc "Join the given ratelimiter to the group."
  @spec join(pid()) :: :ok
  def join(pid) do
    :pg.join(@scope_name, @group_name, pid)
  end

  # Supervisor API
  def start_link(_opts) do
    :pg.start_link(@scope_name)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end
end
