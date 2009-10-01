% this one just matches artist:metallica to a hardcoded path.

-module(fake_resolver).

-behaviour(gen_server).

%% API
-export([start_link/0, resolve/2, weight/0, targettime/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {}).

%% API
start_link()            -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
resolve(Q, Qpid)        -> gen_server:cast(?MODULE, {resolve, Q, Qpid}).
weight()                -> 100.
targettime()            -> 20.

%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({resolve, Q, Qpid}, State) ->
    case Q of
        {struct, Mq} -> % Mq is a proplist
            case string:to_lower(
                 binary_to_list(proplists:get_value(<<"artist">>, Mq))) of
                "metallica" ->
                    Rep =   {struct, [
                                {<<"artist">>, <<"Metallica">>},
                                {<<"track">>,  <<"Enter Sandman">>},
                                {<<"album">>,  <<"Self-titled">>},
                                {<<"mimetype">>, <<"audio/mpeg">>},
                                {<<"score">>, 0.9},
                                {<<"url">>, <<"file:///home/rj/oldhome/rj/mp3/The Commitments/01 Mustang Sally.mp3">>}
                                
                            ]},
                    qry:add_result(Qpid, Rep);
                _ -> io:format("No results from fake_resolver~n",[])
            end;
        _ -> io:format("Unhandled query type in fake_resolver~n",[])
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

