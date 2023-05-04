# Contributing to Nostrum
👍🎉 First off, thanks for taking the time to contribute! 🎉👍

The following is a set of guidelines for contributing to Nostrum. These are just
guidelines, not rules. Use your best judgment, and feel free to propose changes
to this document in a pull request.

## Style
### Piping
When using the pipe operator `|>`, adhere to the following format
```elixir
request.route
|> major_parameter
|> Bucket.get_ratelimit_timeout
```

If you want to assign the result of piping, indent the rhs as such
```elixir
retry_time = 
  request.route
  |> major_parameter
  |> Bucket.get_ratelimit_timeout
```

## Issues and Pull Requests
Before you submit any code, please run the following commands from your terminal.

```elixir
mix compile --force
```
When applicable, please fix any warnings this brings up.

```elixir
mix credo --strict
```
If there are any warnings please handle them to the best of your ability. If you're
unsure of what anything means, feel free to ask or consult the credo docs. As of the
time of this writing, there are multiple non-warning issues that still need to be
addressed, but those are much lower priority.
