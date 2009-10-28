% Erlang audioscrobbler client.
% no error handling yet, it'll just crash on unexpected server responses.
-module(erlscrobbler).
-include("playdar.hrl").
-behaviour(gen_server).

%% API
-export([start_link/2, md5/1, np/1, submit/1, 
         start/4, start/5, stop/0, pause/0, resume/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(CLIENTID, 	"tst").
-define(CLIENTVER, 	"0.1").
-define(ASBASE, 	"http://post.audioscrobbler.com").

-record(state, {hs, username, password, session, npurl, suburl, current, tref, ticks}).
-record(track, {artist, album, track, trackno, secs, time, o}).

%% API
start_link(U,P) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [U,md5(P)], []).

np(T) -> gen_server:cast(?MODULE, {np, T}).
submit(T) -> gen_server:cast(?MODULE, {submit, T}).

start(Art, Alb, Trk, Len)    -> start(Art, Alb, Trk, Len, "P").
start(Art, Alb, Trk, Len, O) -> 
    T = #track{artist=Art, album=Alb, track=Trk, secs=Len, trackno=0, time=ts(), o=O},
    gen_server:cast(?MODULE, {start, T}).

stop()   -> gen_server:cast(?MODULE, stop).
pause()  -> gen_server:cast(?MODULE, pause).
resume() -> gen_server:cast(?MODULE, resume).


%% gen_server callbacks


init([U,P]) ->
    inets:start(),
    spawn( fun() -> gen_server:cast(?MODULE, handshake) end ),
    {ok, #state{hs=false, username=U, password=P, current=undefined}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({start, T}, State) ->
    timer:cancel(State#state.tref),
    {ok, Tref} = timer:send_interval(1000, self(), tick),
    {noreply, State#state{tref=Tref, ticks=0, current=T}};

handle_cast(pause, State) ->
    timer:cancel(State#state.tref),
    {noreply, State#state{tref=undefined}};

handle_cast(resume, State) ->
    timer:cancel(State#state.tref),
    {ok, Tref} = timer:send_interval(1000, self(), tick),
    {noreply, State#state{tref=Tref}};

handle_cast(stop, State) ->
    timer:cancel(State#state.tref),
    {noreply, State#state{tref=undefined, ticks=0}};

handle_cast(handshake, State = #state{ hs = false }) ->
    Ts = ts(),
    Auth = md5( io_lib:format("~s~w",[State#state.password, Ts]) ),
    Url = io_lib:format("/?hs=true&p=1.2.1&c=~s&v=~s&u=~s&t=~w&a=~s",
                        [?CLIENTID, ?CLIENTVER, State#state.username, Ts, Auth]),
    case http_get(?ASBASE ++ Url) of
        {ok,{_, _, Body}} ->
            ["OK", Session, NpUrl, SubUrl] = string:tokens(Body, "\n"),
            io:format("Session: ~s, NP: ~s, Sub: ~s~n", [Session, NpUrl, SubUrl]),
            {noreply, State#state{ session=Session, npurl=NpUrl, suburl=SubUrl, hs=true}}
    end;

handle_cast({np, #track{artist=Artist, album=Album, track=Track, secs=Secs, trackno=TrackNo}}, State) ->
    Params = [
        {"s", State#state.session},
        {"a", Artist},
        {"b", Album},
        {"t", Track},
        {"l", Secs},
        {"n", TrackNo},
        {"m", ""}
    ],
    {ok, {_,_,Body}} = http_post(State#state.npurl, Params),
    case string:tokens(Body, "\n") of
        ["OK"|_] ->
			?LOG(info, "Sent Now-Playing",[]),
            {noreply, State};
        _ ->
            ?LOG(warning, "Failed to send Now-Playing",[]),
            {noreply, State}
    end;

handle_cast({submit, T}, State) ->
    Params = [
        {"s", State#state.session},
        {"a[0]", T#track.artist},
        {"b[0]", T#track.album},
        {"t[0]", T#track.track},
        {"l[0]", T#track.secs},
        {"n[0]", T#track.trackno},
        {"m[0]", ""},
        {"r[0]", ""},
        {"o[0]", "P"},
        {"i[0]", T#track.time}
    ],
    {ok, {_,_,Body}} = http_post(State#state.suburl, Params),
    case string:tokens(Body, "\n") of
        ["OK"|_] ->
            ?LOG(info, "Scrobbled!", []),
            {noreply, State};
        _ ->
            ?LOG(warning, "Failed to scrobble :(", []),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State = #state{ticks=4, current=C}) when is_record(C, track) ->
    ?MODULE:np(State#state.current),
    {noreply, State#state{ticks=5}};

handle_info(tick, State) ->
    Ticks = State#state.ticks + 1,
    T = State#state.current,
    Trigger0  =round(T#track.secs/2),
    Trigger = if 
                (Trigger0 > 0) and (Trigger0 < 200) -> Trigger0;
                true -> 200
              end,
    case Ticks >= Trigger of
        true ->
            ?MODULE:submit(State#state.current),
            timer:cancel(State#state.tref),
            {noreply, State#state{ticks=0, tref=undefined}};
        false ->
            {noreply, State#state{ticks=Ticks}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
ts() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.

http_post(Url, Params) ->
    Body = lists:flatten([ esc(K) ++ "=" ++ esc(V) ++ "&" || {K,V} <- Params ]),
    http:request(post, {Url, [], "application/x-www-form-urlencoded", Body}, [], [{sync, true}, {version, 1.1}]).


http_get(Url) ->
    io:format("GET ~s~n", [Url]),
    http:request(get, {Url, []}, [], [{sync, true}, {version, 1.1}]).

md5(Str) ->
    string:to_lower(
        lists:flatten(
            [io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(Str))]
    )).

esc(Str) when is_list(Str) -> edoc_lib:escape_uri(Str);
esc(Str) when is_integer(Str) -> integer_to_list(Str).
