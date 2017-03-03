# Events
Event handling is how your bot application will interact with the information
sent from Discord over a websocket connection. By defining an event handler for
an event, when something like a message is created or a channel is deleted, your
application can perform an action as a result of that event.

Mixcord currently uses a [GenStage](https://github.com/elixir-lang/gen_stage)
implementation to handle dispatching events from the WS connection. To handle
events it is up to you to define the `consumer` of the GenStage life cycle.

To see the documentation on implementing that consumer, please see
`Mixcord.Consumer`.

## Why GenStage?
From the GenStage docs:
> GenStage is a specification for exchanging events between producers and consumers.

Sounds a lot like the use case here, doesn't it?

An earlier implementation sent the dispatch events from the websocket directly to
user defined handlers inside of a task. This was okay, but it made handling the
events internally a bit difficult as we would either need to block the main shard
process or daisy chain tasks together. We could use a simple GenServer approach,
but at that point, why not just use GenStage? GenStage users GenServer behind the
scenes, but abstracts away dispatching the data and providing back-pressure.

With the GenStage approach we have an OTP implementation of our required behaviour
that gives us separate maintainable parts with back-pressure.

## Implementation Details
The following information will define the general procedure of dispatching events.

If you disagree with anything listed below or would like to offer a suggestion on
how it can be improved, please feel free to voice your opinion.

### Shard
Every shard in the Mixcord application is ran as its own process. These shards
are all run under the same supervisor. As part of the setup for each shard, it
creates its own producer under a supervisor.

As events are sent to the shard, the following happens:
 1. Shard looks to see what type of event it is, only dispatch events are send to
 the producer.

 2. If the event is a `Dispatch`, the payload is converted to a an atom-keyed map.
 This is done because over ETF (which Mixcord uses), map keys are sometimes
 binaries and sometimes strings, making it a real headache. Additionally, with
 atom keys, we can use the `Map.key` notation. This is normally considered unsafe
 but a warning will be emitted if a key is unsafely converted to an atom. In this
 way we can ensure that our atom table is not growing unbounded.

 3. The payload is then sent to the producer.

### Producer
The producer is responsible for handling the events internally, as well as
dispatching the events.

 1. The producer sends the event to the relevant cache to update its state.

 2. The cache sends back the curated payload information. In the case of updates,
 the cache will sometimes send two payloads back, the old and the new payload,
 for comparison purposes. In general, the payload will often match the payload
 described by the official Discord API documentation.

 3. The producer takes the payload(s) and puts them into a queue of events to send
 to a consumer.

### Consumer
The consumer is implemented by the user. They can spawn any number of consumers
which will in turn request events from the producers created by each shard.

Mixcord wraps the GenStage module to provide a simpler interface for event handling.
One of the benefits to this approach is that we can abstract away the manual linking
of producer to consumer. Additionally, in the future if we had a reason for a
`producer-consumer` it would be trivial to implement without breaking user code.

 1. As events come into the consumer, they're passed to the user defined handlers.
