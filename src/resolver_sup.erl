% Supervisor that the resolver dynamically adds children to
-module(resolver_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Resolver    = { resolver, 
                    {resolver, start_link, []},
                    permanent, 10, worker, [] },
    
    {ok, {{one_for_one, 10, 10}, [Resolver]}}.