import Config

config :logger, :console, metadata: [:shard, :guild, :channel, :bot]

if File.exists?("config/secret.exs"), do: import_config("secret.exs")

if Mix.env() == :test do
  config :nostrum,
    # constrain the size of the message cache in tests
    # to make it easier to test eviction
    # and to make sure we don't accidentally modify a production table
    # when running tests, use a different table name
    caches: [
      messages:
        {Nostrum.Cache.MessageCache.Noop,
         size_limit: 10, eviction_count: 4, table_name: :nostrum_messages_test}
    ],
    streamlink: false,
    youtubedl: false
end
