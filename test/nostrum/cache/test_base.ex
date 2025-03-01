defmodule Nostrum.Cache.TestBase do
  use ExUnit.Case
  alias Nostrum.Bot
  import Nostrum.Bot, only: [set_bot_name: 1, with_bot: 2]

  @doc """
  Set up and tear down the given cache.

  Use in your test like this:

  ```elixir
  setup do
    Nostrum.Cache.TestBase.setup_and_teardown_cache(cache_module)
  end
  ```

  ## Return value

  Returns a keyword list with the following values:

  - `pid` the cache process that was started.
  - `bot_name` the generated name of the bot.
  - `id` the unique identifier generated for the bot name. If you use
  `setup_and_teardown_cache/3` to start other caches in your test that wish to
  call out to the "parent" cache in your test, you need to pass this value
  along.
  """
  @doc since: "0.11.0"
  @spec setup_and_teardown_cache(module()) :: [
          {:pid, pid()},
          {:bot_name, Bot.name()},
          {:cache_id, integer()}
        ]
  def setup_and_teardown_cache(cache) do
    setup_and_teardown_cache(cache, cache, :erlang.unique_integer([:positive]))
  end

  @doc """
  Same as `setup_and_teardown_cache/1`, but allows you to customize the cache
  module interpolated when generating the bot name and the unique identifier.

  This is useful in case you are starting another cache in your cache test and
  want it to use a name appropriate for your currently tested cache.

  Since this function will add a unique integer to generated bot names to ensure
  test isolation within caches, calls to this function must manually pass the 
  """
  @doc since: "0.11.0"
  @spec setup_and_teardown_cache(module(), module(), integer()) :: [
          {:pid, pid()},
          {:bot_name, Bot.name()},
          {:cache_id, integer()}
        ]
  def setup_and_teardown_cache(cache, cache_for_name, cache_id) do
    bot_name = :"#{cache_for_name}test#{cache_id}"

    if function_exported?(cache, :teardown, 0) do
      on_exit(:cleanup, fn ->
        with_bot(bot_name, fn ->
          apply(cache, :teardown, [])
        end)
      end)
    end

    set_bot_name(bot_name)

    spec = {cache, name: bot_name}
    [pid: start_supervised!(spec), bot_name: bot_name, cache_id: cache_id]
  end
end
