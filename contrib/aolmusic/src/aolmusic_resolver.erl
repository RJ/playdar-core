% API used by music.aol.com/song/
%
% See: http://music.aol.com/api/audio/search?start=0&count=20&artistName=metallica&songTitle=enter%20sandman
%
-module(aolmusic_resolver).

-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/2, weight/1, targettime/1, name/1, localonly/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {}).

%%

start_link()            -> gen_server:start_link(?MODULE, [], []).
  
resolve(Pid, Qry)       -> gen_server:cast(Pid, {resolve, Qry}).
weight(_Pid)            -> 100.
targettime(_Pid)        -> 1000.
name(_Pid)              -> "AOL Music Index".
localonly(_Pid)			-> true.

%% --------------------------------------------------------------------
init([]) ->
	playdar_resolver:add_resolver(?MODULE, self()),
    {ok, #state{}}.

handle_cast({resolve, Qry}, State) ->
    {struct, Q} = Qry#qry.obj,
    Art = binary_to_list(proplists:get_value(<<"artist">>, Q, <<"">>)),
    Trk = binary_to_list(proplists:get_value(<<"track">>, Q, <<"">>)),
    Url = io_lib:format("http://music.aol.com/api/audio/search?start=0&count=20&artistName=~s&songTitle=~s",
                        [edoc_lib:escape_uri(Art), edoc_lib:escape_uri(Trk)]),
    {ok,{_, _, Body}} = http_get(Url),
    {struct, J} = mochijson2:decode(Body),
    {struct, J2} = proplists:get_value(<<"response">>, J, []),
    {struct, J3} = proplists:get_value(<<"data">>, J2, []),
    Numfound = proplists:get_value(<<"totalFound">>, J3),
    ?LOG(info, "AOL found ~w potential results", [Numfound]),
    J4 = proplists:get_value(<<"assets">>, J3, []),
    J5 = lists:filter(fun({struct, E}) -> 
                        lists:suffix(".mp3", string:to_lower(
                                                binary_to_list(
                                                    proplists:get_value(<<"enclosure">>, E, <<"">>))))
                      end, J4),
    R = [ begin
            {Dur, _} = string:to_integer(binary_to_list(proplists:get_value(<<"duration">>, First, <<"0">>))),
            {struct, [  {<<"artist">>, proplists:get_value(<<"artistname">>, First, <<"">>)},
                            {<<"track">>, proplists:get_value(<<"songtitle">>, First, <<"">>)},
                            {<<"album">>, proplists:get_value(<<"albumname">>, First, <<"">>)},
                            {<<"duration">>, Dur},
                            {<<"url">>, proplists:get_value(<<"enclosure">>, First, <<"">>)},
                            {<<"source">>, <<"AOL Music">>}
                     ]}
            end || {struct, First} <- J5 ],
    playdar_resolver:add_results(Qry#qry.qid, R),
	{noreply, State}.

handle_call(_Msg, _From, State) -> {reply, not_implemented, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
http_get(Url) ->
    %io:format("GET ~s~n", [Url]),
    http:request(get, {Url, []}, [], [{sync, true}, {version, 1.1}]).
