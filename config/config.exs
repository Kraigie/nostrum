import Config

config :nostrum,
  token: "",
  num_shards: :auto

config :logger, :console, metadata: [:shard]

if File.exists?("config/secret.exs"), do: import_config("secret.exs")
