# Events
Event handling is how your bot application will interact with the information
sent from Discord over a websocket connection. By defining an event handler for
an event, when something like a message is created or a channel is deleted, your
application can perform an action as a result of that event.

Nostrum uses [Erlang's `:pg` module](https://www.erlang.org/doc/man/pg.html) to
determine which consumers are interested in events, via
`Nostrum.ConsumerGroup`. This allows dynamic subscriptions at runtime, even
across nodes. Events are dispatched to group members as they appear fromthe
Discord Gateway after ingestion into the cache.

To see the documentation on using one of provided consumers, please see
`Nostrum.Consumer`.

## Implementation Details
The following information will define the general procedure of dispatching events.

### Shard
Every shard in the Nostrum application is ran as its own process. These shards
are all run under the same supervisor. As part of the setup for each shard, it
creates its own producer under a supervisor.

As events are sent to the shard, the following happens:

1. Shard looks to see what type of event it is, only dispatch events are sent
   to the producer.

2. If the event is a `Dispatch`, the payload is converted to an atom-keyed map.
   This is done because over ETF (which Nostrum uses), map keys are sometimes
   binaries and sometimes strings, making it a real headache. Additionally,
   with atom keys, we can use the `Map.key` notation. This is normally
   considered unsafe but a debug messages will be emitted if a key is unsafely
   converted to an atom. In this way we can ensure that our atom table is not
   growing unbounded.

3. The payload is then written to the cache. To make sure we're not overrunning
   the cache, especially at startup with `request_guild_members` or other heavy
   payloads, this is done in the shard itself. 

4. The cache updates itself from the new data. In some cases, such as update or
   delete events, it may send out a second "old" object as well, that helps the
   library user to determine what changed.

5. After writing to the cache, the shard `send`s out the event after going
   through the cache to all subscribed processes. In general, the payload will
   often match the payload described by the official Discord API documentation.

6. The shard instructs the websocket client that it's ready to read more data.
   This prevents flooding the shard with messages that it may not be able to
   handle yet, thus growing the message queue and the memory usage.


### Consumer

The consumer is implemented by you. Nostrum's `use Nostrum.Consumer` will do
the bulk of the work of process setup. Note that every process that is
subscribed receives every event: it is therefore not recommended to create
multiple consumers if a single one could accomplish the job.

By default, the consumer will start a new process for every event. This allows
us to easily distribute the load across cores.

Nostrum wraps the `:pg` module to provide a straightforward interface for event
handling. One of the benefits to this approach is that we can abstract away the
manual linking of producer to consumer.
