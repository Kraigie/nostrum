# See the appup cookbook for instructions:
# https://www.erlang.org/doc/design_principles/appup_cookbook.html
{
  '0.9.0-alpha2',
  [
    # Upgrade instructions
    {'0.9.0-alpha1',
     [
       {:load_module, Nostrum.Cache.GuildCache}
     ]},
    {'0.8.0',
     [
       {:restart_application, :nostrum}
     ]}
  ],
  [
    # Downgrade instructions
    {'0.9.0-alpha1',
     [
       {:load_module, Nostrum.Cache.GuildCache}
     ]},
    {'0.8.0',
     [
       {:restart_application, :nostrum}
     ]}
  ]
}
