## Consumers
Nostrum uses GenStage for event dispatching. Two different types of consumers
are provided by the library - `Nostrum.Consumer`, and `Nostrum.TaskedConsumer`.
These provided processes abstract most of the "work" of defining consumers away
from the user. With these consumers everything is handled including maintaing all
of the internal caches.

That said, it is still possible to define your own consumers, bypassing maintaing
state within the lib itself.

## Implementing Custom Consumers
To implement your own custom consumer you need to enable the option in your
config. To do so, simply add `custom_consumer: false` to your config.

To find the pids of the producers you need to subscribe to, you can use the
method `Nostrum.Util.producers/0`.
