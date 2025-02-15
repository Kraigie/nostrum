# Manual Sharding

Advanced users can use methods located in the `Nostrum.Shard.Supervisor` module
to manually connect shards to the gateway as well as initiate manual disconnect
and reconnects (attempting to `RESUME` sessions where possible).

You can set the `num_shards` option in your `nostrum` application config to
`:manual` to prevent Nostrum from automatically starting shards. You should use
the methods in the shard supervisor such as `Nostrum.Shard.Supervisor.connect/3`
to manually start shards if using this configuration option.

## Reconnection example

```elixir
# On Node A
iex> bot_options = %{
...>   consumer: MyBot.Consumer,
...>   wrapped_token: fn -> System.get_env!("BOT_TOKEN") end
...> }
iex> Nostrum.Shard.Supervisor.connect(0, 1, bot_options)
iex> resume_info = Nostrum.Shard.Supervisor.disconnect(0)
%{shard_num: 0, ...}

# On another node
iex> Nostrum.Shard.Supervisor.reconnect(resume_info)
```

Discord will perform a best effort attempt to resume the gateway from the time
of disconnection, relaying any missed events. Resumption is not guaranteed to
work, in the event it fails Nostrum will reconnect the shard from scratch,
though this may mean some events are missed.
