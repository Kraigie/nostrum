import Config

config :nostrum,
  token: "",
  num_shards: :auto,
  ffmpeg: "ffmpeg"

config :logger, :console, metadata: [:shard]

config :porcelain, :driver, Porcelain.Driver.Basic

if File.exists?("config/secret.exs"), do: import_config("secret.exs")
