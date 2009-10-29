% Supervisor that the resolver dynamically adds children to
-module(playdar_resolver_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    Resolver    = { playdar_resolver, 
                    {playdar_resolver, start_link, []},
                    permanent, 10, worker, [playdar_resolver] },
    
    {ok, {{one_for_one, 10, 10}, [Resolver]}}.