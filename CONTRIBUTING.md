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

Before you submit any code, please run the following commands from your terminal:

- `mix compile --force`
- `mix format`
- `mix credo --strict --ignore 'TagTODO'`
- `mix test --no-start`
- `mix dialyzer`

or simply:

```sh
mix compile --force && mix format && mix credo --strict --ignore 'TagTODO' && mix test --no-start && mix dialyzer
```

Please fix any warnings that appear.
