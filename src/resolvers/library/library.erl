-module(library).
-behaviour(gen_server).
-behaviour(playdar_resolver).
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0, resolve/3, weight/1, targettime/1, name/1]).
-export([scan/2, generate_index/1 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%-export([ngram/1]).
%-export([ngram/2]).
%-export([list_agg/1, list_add/1]).

%% Records
-record(state, {scanner}).
-record(file, {url,
               artist, album, track, 
               artist_clean, album_clean, track_clean,
               size, mtime, hash, mimetype, duration, bitrate, trackno}).
-record(ngram, {gram, name, num}).

%%

start_link()        -> gen_server:start_link(?MODULE, [], []).
scan(Pid, Dir)      -> gen_server:cast(Pid, {scan, Dir}).
generate_index(Pid) -> gen_server:cast(Pid, {generate_index}).

resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(_Pid)            -> 100.
targettime(_Pid)        -> 20.
name(_Pid)              -> "Local Library".

%%

init([]) ->
    ok = mnesia:start(),
    mnesia:wait_for_tables([file, ngram], 30000),
    try
        Info = mnesia:table_info(file, size),
        io:format("Library contains ~w files\n",[Info])
    catch
        exit:_Why -> % because it doesnt exist yet?
            first_run();
        X ->
            io:format("Mnesia table_info failed somehow.\n",[]),
            throw(X)
    end,    
    resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
    {ok, #state{scanner=undefined}}.



handle_cast({resolve, Q, Qpid}, State) ->
    io:format("library:resolver~n",[]),
    case Q of
        {struct, Mq} -> % Mq is a proplist
            Report = fun(Score, File) ->
                Rep =   {struct, [
                                {<<"artist">>, File#file.artist},
                                {<<"track">>,  File#file.track},
                                {<<"album">>,  File#file.album},
                                {<<"mimetype">>, File#file.mimetype},
                                {<<"score">>, Score},
                                {<<"url">>, File#file.url},
                                {<<"duration">>, File#file.duration},
                                {<<"bitrate">>, File#file.bitrate},
                                {<<"size">>, File#file.size}
                            ]},
                qry:add_result(Qpid, Rep)                
            end,
            Art = proplists:get_value(<<"artist">>,Mq,<<"">>),
            Trk = proplists:get_value(<<"track">>,Mq,<<"">>),
            Alb = proplists:get_value(<<"album">>,Mq,<<"">>),
            Now = now(),
            Files = search(clean(Art),clean(Alb),clean(Trk)),
            Time = timer:now_diff(now(), Now),
            io:format("Library:search took: ~wms~n",[Time/1000]),
            [ Report(Score, File) || {Score, File} <- Files ];

        _ -> io:format("Unhandled query type in library resolver~n",[])
    end,
    {noreply, State};

handle_cast({generate_index}, State) ->
    {atomic, L} = mnesia:transaction(fun()-> qlc:e(qlc:q(
                       [ {string:to_lower(binary_to_list(F#file.artist)), 
                          string:to_lower(binary_to_list(F#file.track))}
                         || F <- mnesia:table(file) 
                       ] )) end),
    Larts = [N || {N,_} <- lists:ukeysort(1, L)],
    Ltrks = [N || {_,N} <- lists:ukeysort(2, L)],
    Fun = fun(Name, Type)->
                        Ns = [ #ngram{gram={Type, Gram}, name=Name, num=Num} 
                               || {Gram, Num} <- ngram(Name)],
                        lists:foreach(fun(E)->mnesia:dirty_write(E)end, Ns)
          end,
    lists:foreach(fun(E)->Fun(E,artist)end, Larts),
    lists:foreach(fun(E)->Fun(E,track)end, Ltrks),
    io:format("Finished indexing~n",[]),
    {noreply, State};

handle_cast({scan, Dir}, State) ->
    Pid = spawn_link(scanner, scan_dir, [Dir, self()]),
    {noreply, State#state{scanner=Pid}}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info({'EXIT', Pid, Reason}, #state{scanner = Pid} = State) ->
    io:format("Scanner crashed: ~w~n", [Reason]),
    {noreply, State#state{scanner=undefined}};

handle_info({scanner, finished}, State) -> 
    io:format  ("Scan finished!~n",[]),
    generate_index(self()),
    {noreply, State#state{scanner=undefined}};

handle_info({scanner, {file, File, Mtime, Tags}}, State) when is_list(Tags), is_list(File) ->
    case proplists:get_value(<<"error">>, Tags) of
        undefined ->
            Art = proplists:get_value(<<"artist">>, Tags, <<"">>),
            Alb = proplists:get_value(<<"album">>, Tags, <<"">>),
            Trk = proplists:get_value(<<"track">>, Tags, <<"">>),
            Artist = clean(Art),
            Album  = clean(Alb),
            Track  = clean(Trk),
            F = #file{  url     = proplists:get_value(<<"url">>, Tags, <<"">>),
                        artist  = Art,
                        album   = Alb,
                        track   = Trk,
                        artist_clean = Artist,
                        album_clean = Album,
                        track_clean = Track,
                        hash    = proplists:get_value(<<"hash">>, Tags, <<"">>),
                        mimetype= proplists:get_value(<<"mimetype">>, Tags, <<"">>),
                        size    = proplists:get_value(<<"filesize">>, Tags, 0),
                        trackno = proplists:get_value(<<"trackno">>, Tags, 0),
                        bitrate = proplists:get_value(<<"bitrate">>, Tags, 0),
                        duration= proplists:get_value(<<"duration">>, Tags, 0),
                        mtime   = Mtime
                     },
            mnesia:dirty_write(F),
            Artist_Ngrams = [ #ngram{gram={artist, Gram}, name=Artist, num=Num} 
                              || {Gram, Num} <- ngram(Artist)], 
            Track_Ngrams  = [ #ngram{gram={track, Gram}, name=Track, num=Num} 
                              || {Gram, Num} <- ngram(Track)],
            Ngrams = Artist_Ngrams ++ Track_Ngrams,
            lists:foreach(fun(E)->mnesia:dirty_write(E)end, Ngrams);
        _Err ->
            noop
    end,

    {noreply, State}.


terminate(_Reason, _State) ->
    mnesia:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%TODO remove punctuation, normalize etc:
clean(Name) when is_binary(Name) -> string:to_lower(binary_to_list(Name)).

% ngram("abcdabcd") -> [{"dab",1},{"cda",1},{"bcd",2},{"abc",2}]
ngram( In )                     when is_list(In)     -> list_agg(lists:sort(ngram( string:to_lower(In), [] ))).
ngram( "", Ngrams )                                  -> Ngrams;
ngram( [A,B,C|Rem]=In, Ngrams ) when length(In) > 3  -> ngram( [B,C|Rem], [ [A,B,C] | Ngrams ] );
ngram( In, Ngrams )             when length(In) =< 3 -> ngram( "", [ In | Ngrams ] ).

% list_agg([a,a,a,b,b,c]) -> [{a,3}, {b,2}, {c,1}]
list_agg([H|T])     -> list_agg(T, [{H,1}]).
list_agg([], Agg)   -> Agg;
list_agg([H|T], [{Ah,An}|At])     when H == Ah  -> list_agg(T, [{Ah,An+1}|At]);
list_agg([H|T], [{Ah,_An}|_At]=Agg) when H /= Ah  -> list_agg(T, [{H,1}|Agg]).
 

%[{X,1},{X,2}] -> [{X,3}]
list_add([]) -> [];
list_add([{Lk,Lv}|Lt]) -> list_add(Lt,[{Lk,Lv}]).
list_add([],Agg) -> Agg;
list_add([{Hk,Hv} | T], [{Hk,Av}|At]) -> list_add(T, [{Hk,Hv+Av}|At]);
list_add([{Hk,Hv} | T], [{_Ak,_Av}|_]=Agg) -> list_add(T, [{Hk, Hv}|Agg]).

% Search mnesia database for best results
search(Art,_Alb,Trk) ->
    L = [{artist, N} || {N,_Num} <- ngram(Art)] ++
        [{track,  N} || {N,_Num} <- ngram(Trk)],
    {atomic, C} = mnesia:transaction(fun()-> 
                    qlc:e(qlc:q(
                     [{N#ngram.gram, N#ngram.name, N#ngram.num} 
                      || N <- mnesia:table(ngram), lists:member(N#ngram.gram, L) 
                     ])) end),
    ArtCands = list_add(lists:keysort(1, [ {Name1,Num1} 
                                     || {{artist,_}, Name1, Num1}<- C ])),
    TrkCands = list_add(lists:keysort(1, [ {Name2, Num2} 
                                     || {{track,_}, Name2, Num2}<- C ])),
    % now get actual files that have these artst/track names
    {atomic, F} = mnesia:transaction(fun()-> 
                    qlc:e(qlc:q(
                     [E || E <- mnesia:table(file), 
                                lists:member(E#file.artist_clean, [A||{A,_N}<-ArtCands]), 
                                lists:member(E#file.track_clean, [A||{A,_N}<-TrkCands]) 
                     ])) end),
    
    % final scoring
    Results = [ begin
                    ArtDist = levenshtein(Art, File#file.artist_clean),
                    TrkDist = levenshtein(Trk, File#file.track_clean),
                    %TLen = length(Art)+length(Trk),
                    TLen = length(File#file.artist_clean)+length(File#file.track_clean),
                    TDist = utils:min(TLen, ArtDist + TrkDist),
                    Score = (TLen - TDist)/TLen,                    
                    %{ ArtDist,TrkDist, TLen, TDist, Score, File }
                    { Score, File }
                end || File <- F ],
    
    lists:sublist(lists:reverse(lists:keysort(1, Results)),10).

% Mnesia setup:
first_run() ->
    (catch mnesia:stop()),
    mnesia:create_schema([node()]),
    io:format("Starting mnesia..\n",[]),
    mnesia:start(),
    io:format("Creating table 'file'..\n",[]),
    mnesia:create_table(file,
            [
                {disc_copies, [node()]},
                {attributes, record_info(fields, file)},
                {index, [artist, track]}, 
                {type, set}
            ]
            ),
    io:format("Creating table 'ngram'..\n",[]),
    mnesia:create_table(ngram,
            [
                {disc_copies, [node()]},
                {attributes, record_info(fields, ngram)},
                {index, []}, 
                {type, bag}
            ]
            ),
    io:format("OK\n",[]),
    mnesia:start(),
    ok.

% edit dist
levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
    levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
    levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
    lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->
    Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
    levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
    NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.
