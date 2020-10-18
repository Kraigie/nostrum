import Config

config :nostrum,
  token: "",
  num_shards: :auto,
  ffmpeg: "ffmpeg",
  youtubedl: "youtube-dl"

config :logger, :console, metadata: [:shard, :guild, :channel]

config :porcelain, :driver, Porcelain.Driver.Basic

if File.exists?("config/secret.exs"), do: import_config("secret.exs")
