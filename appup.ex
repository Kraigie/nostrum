# See the appup cookbook for instructions:
# https://www.erlang.org/doc/design_principles/appup_cookbook.html
{
  ~c"0.9.0",
  [
    # Upgrade instructions
    {~c"0.9.0-alpha3",
     [
       # needed to add back http1.1 support
       {:restart_application, :nostrum}
     ]},
    {~c"0.9.0-alpha2", []},
    {~c"0.9.0-alpha1",
     [
       # Top shard supervisor was not registered, so could not restart shard
       # supervisor to load new gateway logic
       {:restart_application, :nostrum}
     ]},
    {~c"0.8.0",
     [
       {:restart_application, :nostrum}
     ]}
  ],
  [
    # Downgrade instructions
    {~c"0.9.0-alpha3",
     [
       {:restart_application, :nostrum}
     ]},
    {~c"0.9.0-alpha2", []},
    {~c"0.9.0-alpha1",
     [
       {:restart_application, :nostrum}
     ]},
    {~c"0.8.0",
     [
       {:restart_application, :nostrum}
     ]}
  ]
}
