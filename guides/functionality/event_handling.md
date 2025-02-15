# Event handling

Event handling is how your bot application will interact with the information
sent from Discord over a websocket connection. By defining an event handler for
an event, when something like a message is created or a channel is deleted, your
application can perform an action as a result of that event.

nostrum will dispatch all events on your bot to the consumer configured via
`t:Nostrum.Bot.bot_options/0`. Furthermore, nostrum supports listening to events
via `Nostrum.ConsumerGroup`.  This allows dynamic subscriptions at runtime, even
across nodes. Events are dispatched to the configured consumer and any listening
processes from the Discord Gateway after they are ingested into the cache.

To see the documentation on handling events of provided consumers, please see
`Nostrum.Consumer`.


<!-- vim: set textwidth=80 sw=2 ts=2: -->
