use Mix.Config

config :nostrum,
  token: "",
  num_shards: 1

config :logger, :console, metadata: [:shard]
