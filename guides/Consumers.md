## Consumers
Nostrum uses GenStage for event dispatching. A consumer is provided by the
library in the form of `Nostrum.Consumer`. This provided process abstracts most
of the "work" of defining consumers away from the user. With this consumer
everything is handled including maintaining all of the internal caches.

That said, it is still possible to define your own consumers, bypassing maintaining
state within the lib itself.

## Implementing Custom Consumers
To implement your own custom consumer you need to enable the option in your
config. To do so, simply add `custom_consumer: false` to your config.
