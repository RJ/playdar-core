-module(player_api).

-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/2, weight/1, targettime/1, name/1, localonly/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([http_req/2]).

%% Records
-record(state, {}).

%%

start_link()            -> gen_server:start_link(?MODULE, [], []).
  
resolve(_Pid, _Qry)       -> ok.
weight(_Pid)            -> 0.
targettime(_Pid)        -> 0.
name(_Pid)              -> "Player".
localonly(_Pid)         -> true.


%% --------------------------------------------------------------------
init([]) ->
    {ok, _} = mplayer_master:start_link(),
    playdar_http_registry:register_handler("player", 
                                           fun player_api:http_req/2, 
                                           "Player API", "/player"),
    {ok, #state{}}.

handle_cast(_Msg, State) -> { noreply, State }.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) ->    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%

http_req(Req, DocRoot) ->
    "/player" ++ Path = Req:get(path), 
    Qs = Req:parse_qs(),
    case proplists:get_value("method", Qs) of
        "play" ->
            case proplists:get_value("sid", Qs) of
                undefined -> Req:not_found();
                Sid ->
                    Url = "http://localhost:60210/sid/"++Sid,
                    mplayer_master:play(Url),
                    R = {struct, [{<<"response">>, <<"ok">>}]},
                    playdar_http_api:respond(Req, R)
            end;
        
        "stop" ->
            mplayer_master:stop(),
            R = {struct, [{<<"response">>, <<"ok">>}]},
            playdar_http_api:respond(Req, R);
        
        "pausetoggle" ->
            mplayer_master:pause(),
            R = {struct, [{<<"response">>, <<"ok">>}]},
            playdar_http_api:respond(Req, R);
        
        "np" ->
            Np = mplayer_master:np(),
            R = {struct, [{<<"response">>, list_to_binary(Np)}]},
            playdar_http_api:respond(Req, R);
        
        "pos" ->
            Pos = mplayer_master:pos(),
            R = {struct, [{<<"response">>, Pos}]},
            playdar_http_api:respond(Req, R);
        
        _ ->
            Req:not_found()
    end.
        
        
