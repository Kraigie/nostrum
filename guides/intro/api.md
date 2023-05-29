# API

nostrum includes a complete implementation of the endpoints supported by
Discord's API. Method names are copied closely from the documentation to
eliminate any confusion about what a method does, as well as to allow users to
easily lookup the endpoints in the official API documentation.

For a full listing of method definitions, please see the `Nostrum.Api` module.


## Banged (`!`) API methods

A lot of methods have a `banged` version of themselves. This is a common Elixir
idiom hailing from Elixir's style of failing fast.

By default, the API method will return a tuple like one of the following:

```elixir
# Success
{:ok, msg} = Nostrum.Api.create_message(179679229036724225, "456")

# Failure
{:error, reason} = Nostrum.Api.create_message(123, "eat my shorts api")
```

A banged method, instead of returning an `error` tuple, will throw an error. If
successful, it will directly return the response with no `:ok` tuple.

```elixir
# Success
msg = Nostrum.Api.create_message!(179679229036724225, "456")

# Failure - Throws an error
Nostrum.Api.create_message!(123, "eat my shorts api")
```


## Abstractions

When appropriate, some helpers are defined to make interacting with the API
simpler. An example of this is `Nostrum.Api.get_channel_messages/3`. By default
this endpoint only allows the retrieval of `100` messages at a time. A general
use case will have a user wanting more messages than that, thus nostrum handles
the retrieval of any number of messages for the user.

There are other endpoints that could use grooming like above, feel free to
suggest an interface and implementation for these methods.


## Ratelimiting

Ratelimiting is handled internally by nostrum, so long as you use the methods
supplied in the `Nostrum.Api` module. This means either calling the methods as
they're given or using `Nostrum.Api.request/4` to call an endpoint.

To ensure that every request is handled properly, no matter if they're called
asynchronously or not, nostrum funnels all requests through the
`Nostrum.Api.Ratelimiter` state machine.


## REST-only

If you only want to use the REST portion of the provided API, the only process
needed is the ratelimiter, which can be manually started by calling
`Nostrum.Api.Ratelimiter.start_link/1`. 

If you don't want to start nostrum, you can add `runtime: false` to the
dependency options. If you're using `mix release`, all `runtime: false` deps
will be excluded from the build, so you'll also need to add `:nostrum` app to
`mix.exs` in `:included_applications` application option or in the `releases`
project option.

<!-- vim: set textwidth=80 sw=2 ts=2: -->
