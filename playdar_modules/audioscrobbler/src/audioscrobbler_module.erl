% this module isn't a resolver, but it pretends it is so it gets autoloaded.
% it registers an http handler for audioscrobbler support.
-module(audioscrobbler_module).
-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/2, weight/1, targettime/1, name/1, localonly/1]).
-export([http_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%-record(state, {}).

start_link()            -> gen_server:start_link({local,?MODULE},?MODULE,[],[]).
resolve(_Pid, _Qry)     -> ok.
weight(_Pid)            -> 0.
targettime(_Pid)        -> 0.
name(_Pid)              -> "audioscrobbler".
localonly(_Pid)			-> true.

%%%%

init([]) ->
	U = ?CONFVAL({as, username},""),
	P = ?CONFVAL({as, password},""),
	case U=="" orelse P=="" of
		true ->
			?LOG(info, "Not starting scrobbling service, no {as, username} and {as, password} specified", []),
			{ok, []};
		false ->
			?LOG(info, "Starting Audioscrobbler service, username: ~s", [U]),
			{ok, _} = erlscrobbler:start_link(U, P),
			playdar_http_registry:register_handler("audioscrobbler", 
										   fun ?MODULE:http_req/2, 
										   "Audioscrobbler support", 
										   "/audioscrobbler"),
			% We don't need to run as a process now that we registered the above:
			% TODO is it ok to return stop?
			{ok, []}
	end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

http_req(Req, _DocRoot) ->
	"/audioscrobbler" ++ Path = Req:get(path), 
    Qs = Req:parse_qs(),
	Auth = playdar_auth:check_auth(proplists:get_value("auth",Qs,"")),
	case Auth of
		undefined ->
			Req:not_found();
		P when is_list(P) -> http_req_authed(Req, Path, Qs)
	end.

http_req_authed(Req, Path, Qs) ->
	Rep = case proplists:get_value("jsonp", Qs) of
			  undefined -> fun(A) -> Req:respond({200, [], "{\"success\":true, \"action\":\"" ++ A ++ "\"}"}) end;
			  Cb -> fun(A) -> Req:respond({200, [], Cb++"({\"success\":true, \"action\":\"" ++ A ++ "\"});"}) end
		  end,
	case Path of
		"/start" ->
			Art = proplists:get_value("a", Qs, ""),
			Alb = proplists:get_value("b", Qs, ""),
			Trk = proplists:get_value("t", Qs, ""),
			Src = proplists:get_value("o", Qs, "P"),
			LenS = proplists:get_value("l", Qs, "0"),
			Len = erlang:list_to_integer(hd(string:tokens(LenS,"."))),
			erlscrobbler:start(Art, Alb, Trk, Len, Src),
			Rep("start");
		"/resume" -> erlscrobbler:resume(), Rep("resume");
		"/pause"  -> erlscrobbler:pause(),  Rep("pause");
		"/stop"   -> erlscrobbler:stop(),   Rep("stop");
		_ -> Req:not_found()
	end.
		