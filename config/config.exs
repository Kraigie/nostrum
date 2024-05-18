import Config

config :nostrum,
  token: ""

config :logger, :console, metadata: [:shard, :guild, :channel]

if File.exists?("config/secret.exs"), do: import_config("secret.exs")

if Mix.env() == :test do
  config :nostrum,
    # constrain the size of the message cache in tests
    # to make it easier to test eviction
    caches: [
      message_cache_size_limit: 10,
      message_cache_eviction_count: 4,
      message_cache_table_name: :nostrum_messages_test
    ]
end
