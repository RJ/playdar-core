-module(playdar_supervisor).                                                                                                            
-behavior(supervisor).                                                                                                                  

-export([start_link/0, start_link/1]).
-export([init/1]).                    

start_link(_Args) -> start_link().

start_link() ->
    supervisor:start_link({local, playdar_supervisor}, playdar_supervisor, []).

atoi(S) ->
    {I, _} = string:to_integer(S),
    I.

init(_Args) ->
%    Adminport = case os:getenv("ADMINPORT") of false -> 8887; Any0 -> atoi(Any0) end,

    % Subsmanager starts mnesia.
    ResolverSpec    = { resolver, {resolver, start_link, []},
                        permanent, 10, worker, [] },

    Specs           = [ ResolverSpec ],

    ok = supervisor:check_childspecs(Specs),

    StartSpecs = {{one_for_one, 2, 10}, Specs},

    {ok, StartSpecs}.

