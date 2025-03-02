# See the appup cookbook for instructions:
# https://www.erlang.org/doc/design_principles/appup_cookbook.html
{
  ~c"0.10.4",
  [
    # Upgrade instructions
    {~c"0.10.3", [{:update, Nostrum.Api.Ratelimiter, {:advanced, []}}]},
    {~c"0.10.2", [{:update, Nostrum.Api.Ratelimiter, {:advanced, []}}]},
    {~c"0.10.1", [{:update, Nostrum.Api.Ratelimiter, {:advanced, []}}]},
    {~c"0.10.0", [{:restart_application, :nostrum}]},
    {~c"0.9.1", [{:restart_application, :nostrum}]},
    {~c"0.9.0", [{:restart_application, :nostrum}]},
    {~c"0.9.0-rc1", [{:restart_application, :nostrum}]},
    {~c"0.9.0-alpha3", [{:restart_application, :nostrum}]},
    {~c"0.9.0-alpha2", [{:restart_application, :nostrum}]},
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
