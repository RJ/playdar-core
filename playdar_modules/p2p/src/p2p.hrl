-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {nodelay,   true},
                   {packet,    4},
                   {reuseaddr, true}]).

-define(TCP_OPTS_SERVER, [ {backlog,   10} | ?TCP_OPTS ]).
