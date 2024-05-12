import Config

config :nostrum,
  token: ""

config :logger, :console, metadata: [:shard, :guild, :channel]

if File.exists?("config/secret.exs"), do: import_config("secret.exs")
