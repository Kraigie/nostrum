# Hot code upgrade

For the library users whose Discord bot can afford absolutely no downtime due to
restarts, nostrum ships with [`appup`
files](https://www.erlang.org/doc/man/appup.html) via the
[`castle`](https://github.com/ausimian/castle) library.


> ### Maintenance {: .info}
> 
> The appups are maintained on a best-effort basis. While they are expected to
> work and be present and documented as such, no guarantee can be made that they
> will choose the most optimal path to upgrade and downgrade releases.
> Contributions in this regard are welcome.


### Usage

To make use of nostrum's hot code upgrade support, please follow the
instructions to install the [`castle`](https://github.com/ausimian/castle)
library in order to configure your release appropriately. Outside of shipping
the appup, nostrum does not provide further functionality to facilitate this.


### nostrum as included application

When nostrum is run as an included application as documented in [the
multi-node support document](./multi_node.md), nostrum's default appup won't
be read on generation of the release upgrade file.

Please see the OTP documentation on [Changing Included
Applications](https://www.erlang.org/doc/design_principles/appup_cookbook.html#changing-included-applications)
for information on how to deal with upgrades in this case.


<!-- vim: set textwidth=80 sw=2 ts=2: -->
