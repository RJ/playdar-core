-module(listener_impl).
-include("p2p.hrl").
-include("playdar.hrl").
%% API
-export([start_link/1]).

start_link(Port) -> spawn_link(fun()->
                                {ok, Sock} = gen_tcp:listen(Port, ?TCP_OPTS_SERVER),
                                do_accept(Sock)
                               end).
    
do_accept(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            ?LOG(info, "handle accept",[]),
            {ok, Pid} = p2p_conn:start(Sock, in),
            gen_tcp:controlling_process(Sock, Pid),
            do_accept(LSock);
        
        {error, Err} ->
            ?LOG(warning, "Couldn't accept incoming connection: ~p", [Err]),
            throw(p2p_listen_accept_error)
    end.