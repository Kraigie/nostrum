# Event handling

Event handling is how your bot application will interact with the information
sent from Discord over a websocket connection. By defining an event handler for
an event, when something like a message is created or a channel is deleted, your
application can perform an action as a result of that event.

Nostrum uses [Erlang's `:pg` module](https://www.erlang.org/doc/man/pg.html) to
determine which consumers are interested in events, via
`Nostrum.ConsumerGroup`. This allows dynamic subscriptions at runtime, even
across nodes. Events are dispatched to group members as they appear fromthe
Discord Gateway after ingestion into the cache.

To see the documentation on handling events of provided consumers, please see
`Nostrum.Consumer`.


<!-- vim: set textwidth=80 sw=2 ts=2: -->
