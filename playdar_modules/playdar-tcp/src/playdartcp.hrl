-define(TCP_OPTS, [binary, inet,
                   {active,    false},
                   {nodelay,   true},
                   {packet,    4},
                   {reuseaddr, true}]).

-define(TCP_OPTS_SERVER, [ {backlog,   10} | ?TCP_OPTS ]).
-define(T2B(T), term_to_binary(T)).
-define(B2T(T), binary_to_term(T)).
