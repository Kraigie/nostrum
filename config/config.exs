import Config

config :nostrum,
  token: "",
  num_shards: :auto

config :logger, :console, metadata: [:shard]
