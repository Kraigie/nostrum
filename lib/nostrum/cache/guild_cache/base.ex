defmodule Nostrum.Cache.GuildCache.Base do
  @moduledoc false
  @moduledoc since: "0.8.0"

  alias Nostrum.Snowflake
  alias Nostrum.Util

  @spec upsert(%{required(Snowflake.t()) => struct}, Snowflake.t(), map, atom) ::
          {struct | nil, struct, %{required(Snowflake.t()) => struct}}
  def upsert(map, key, new, struct) do
    if Map.has_key?(map, key) do
      old = Map.get(map, key)

      new =
        old
        |> Map.from_struct()
        |> Map.merge(new)
        |> Util.cast({:struct, struct})

      new_map = Map.put(map, key, new)

      {old, new, new_map}
    else
      new = Util.cast(new, {:struct, struct})
      {nil, new, Map.put(map, key, new)}
    end
  end
end
