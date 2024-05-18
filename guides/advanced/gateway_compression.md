# Gateway Compression

Nostrum supports either the `zlib-stream` or `zstd-stream` gateway compression
methods, as documented
[here](https://discord.com/developers/docs/topics/gateway#encoding-and-compression)

Most users are fine to leave the `gateway_compression` configuration option set
to `:zlib` (default), but users looking for a potential reduction in payload
sizes from the Discord gateway can optionally set `:zstd` here.

## Using `:zstd` compression

Using `:zstd` depends on the [`:ezstd`](https://hex.pm/packages/ezstd) library,
so you will have to add this dependency to your `mix.exs` file:

```elixir
  defp deps do
    [
      {:nostrum, ...},
      {:ezstd, "~> 1.1"} # new dependency
    ]
  end
```


> #### `:ezstd` NIFs {: .info}
>
> Some functionality of `:ezstd` depends on Erlang NIFs (Natively Implemented
> Functions). This means that a proper compiler installation as well as other
> build tools like `git` may be necessary at the stage where you compile your
> dependencies.
>
> It may be useful to run `mix deps.compile` in any build systems to ensure that
> your application does not need build utilities in the built application image.

Once you have this additional dependency installed in your project, set the
`:nostrum`, `:gateway_compression` configuration option to `:zstd` and Nostrum
should pick up on it.

You will need to run `mix deps.get` and `mix deps.compile` to install and
compile the new `:ezstd` dependency.

> #### Nostrum detection of `:ezstd` {: .tip}
>
> Since the check for `:ezstd` takes place when you compile Nostrum, you might
> need to run `mix deps.compile --force nostrum` to ensure that Nostrum is
> recompiled and recognises the newly installed `:ezstd` dependency.
>
> Not doing this may mean that your compiled Nostrum version is still using
> dummy handlers that will error out even when `:ezstd` is installed.
