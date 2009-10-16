-module(library_dets).

-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/3, weight/1, targettime/1, name/1, dump_library/1]).
-export([scan/2, stats/1, add_file/5, sync/1 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {scanner, ndb, fdb}).

%%

start_link()            -> gen_server:start_link(?MODULE, [], []).
scan(Pid, Dir)          -> gen_server:call(Pid, {scan, Dir}, infinity).

add_file(Pid, F, Mtime, Size, L) -> gen_server:call(Pid, {add_file, F, Mtime, Size, L}, 60000).
sync(Pid)               -> gen_server:cast(Pid, sync).
  
resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(_Pid)            -> 100.
targettime(_Pid)        -> 20.
name(_Pid)              -> "Local Library using DETS".

stats(Pid)              -> gen_server:call(Pid, stats).

dump_library(Pid)       -> gen_server:call(Pid, dump_library, 60000).
%%

init([]) ->
    DbDir = ?CONFVAL({library,dbdir}, "."),
    {ok, Ndb} = dets:open_file(DbDir++"/ngrams.dets",[{type, bag}]),
    {ok, Fdb} = dets:open_file(DbDir++"/files.dets", [{type, set}]),
    ?LOG(info, "Library index contains ~w files", 
               [proplists:get_value(size, dets:info(Fdb), -1)]),
    % start the scanner (kind of a hack, but deadlock if we do it in init here):
    self() ! start_scanner,        
    resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
    {ok, #state{scanner=undefined, ndb=Ndb, fdb=Fdb}}.

handle_cast(sync, State) -> 
    dets:sync(State#state.fdb),
    dets:sync(State#state.ndb),
    ?LOG(info, "library synced", []),
    {noreply, State};

handle_cast({resolve, Q, Qpid}, State) ->
    case Q of
        {struct, Mq} -> % Mq is a proplist
            Hostname = ?CONFVAL(name, "unknown"),
            Report = fun({Props, Score}) ->
                Rep =   {struct, [
                                {<<"artist">>, proplists:get_value(artist, Props)},
                                {<<"track">>,  proplists:get_value(track, Props)},
                                {<<"album">>,  proplists:get_value(album, Props)},
                                {<<"mimetype">>, proplists:get_value(mimetype, Props)},
                                {<<"score">>, Score},
                                {<<"url">>, proplists:get_value(url, Props)},
                                {<<"duration">>, proplists:get_value(duration, Props)},
                                {<<"bitrate">>, proplists:get_value(bitrate, Props)},
                                {<<"size">>, proplists:get_value(size, Props)},
                                {<<"source">>, list_to_binary(Hostname)}
                            ]},
                qry:add_result(Qpid, Rep)                
            end,
            Art = proplists:get_value(<<"artist">>,Mq,<<"">>),
            Trk = proplists:get_value(<<"track">>,Mq,<<"">>),
            Alb = proplists:get_value(<<"album">>,Mq,<<"">>),
            Now = now(),
            Files = search(clean(Art),clean(Alb),clean(Trk),State),
            Time = timer:now_diff(now(), Now),
            ?LOG(info, "Library search took: ~wms for ~s - ~s",[Time/1000, Art, Trk]),
            lists:foreach(Report, Files);

        _ -> noop %Unhandled query type
    end,
    {noreply, State}.

handle_call({add_file, File, Mtime, Size, Tags}, _From, State) when is_list(Tags), is_list(File) ->
    case proplists:get_value(<<"error">>, Tags) of
        undefined ->
            Art = proplists:get_value(<<"artist">>, Tags, <<"">>),
            Alb = proplists:get_value(<<"album">>, Tags, <<"">>),
            Trk = proplists:get_value(<<"track">>, Tags, <<"">>),
            Artist = clean(Art),
            Album  = clean(Alb),
            Track  = clean(Trk),
            FileId = list_to_atom(File), %TODO this is stupid
            Props = [   {url, proplists:get_value(<<"url">>, Tags, <<"">>)},
                        {artist, Art},
                        {album,  Alb},
                        {track,  Trk},
                        {artist_clean,  Artist},
                        {album_clean,   Album},
                        {track_clean,   Track},
                        {hash,          proplists:get_value(<<"hash">>, Tags, <<"">>)},
                        {mimetype,      proplists:get_value(<<"mimetype">>, Tags, <<"">>)},
                        {size,          Size},
                        {trackno,       proplists:get_value(<<"trackno">>, Tags, 0)},
                        {bitrate,       proplists:get_value(<<"bitrate">>, Tags, 0)},
                        {duration,      proplists:get_value(<<"duration">>, Tags, 0)},
                        {mtime,         Mtime}
                    ],
            dets:insert(State#state.fdb, {FileId, Props}),
            Artist_Ngrams = [ {{artist, list_to_atom(Gram)}, FileId} 
                              || {Gram, _Num} <- ngram(Artist)], 
            Track_Ngrams  = [ {{track, list_to_atom(Gram)}, FileId} 
                              || {Gram, _Num} <- ngram(Track)],
            ok = dets:insert(State#state.ndb, Artist_Ngrams),
            ok = dets:insert(State#state.ndb, Track_Ngrams),
            {reply, ok, State};
        _Err ->
            {reply, error, State}
    end;

handle_call({scan, Dir}, From, State) ->
    spawn(fun()->
                  gen_server:reply(From, (catch scanner:scan_dir(State#state.scanner, Dir)))
          end),
    {noreply, State};

handle_call(stats, _From, State) ->
    NumFiles = proplists:get_value(size, dets:info(State#state.fdb), -1),
    {reply, [{num_files, NumFiles}], State};
    
handle_call(dump_library, _From, State) ->
    All = dets:traverse(State#state.fdb, fun({_Fid, X}) -> {continue, X} end),
    {reply, All, State};
    
handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_info(start_scanner, State) ->
    ScannerSpec = {scanner, {scanner, start_link, [self()]}, permanent, brutal_kill, worker, [scanner]},
    {ok, S} = supervisor:start_child(modules_sup, ScannerSpec),
    {noreply, State#state{scanner=S}};

handle_info(_Msg, State) -> {noreply, State}.

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
ngram( In )                     when is_list(In)     -> utils:list_agg(lists:sort(ngram( string:to_lower(In), [] ))).
ngram( "", Ngrams )                                  -> Ngrams;
ngram( [A,B,C|Rem]=In, Ngrams ) when length(In) > 3  -> ngram( [B,C|Rem], [ [A,B,C] | Ngrams ] );
ngram( In, Ngrams )             when length(In) =< 3 -> ngram( "", [ In | Ngrams ] ).



% Lame fuzzy search algorithm using ngrams to find candidates, then
% using edit-distance to rank+score candidates.
search(Art,_Alb,Trk,State) ->
    L = [{artist, list_to_atom(N)} || {N,_Num} <- ngram(Art)] ++
        [{track,  list_to_atom(N)} || {N,_Num} <- ngram(Trk)],
    R = [ begin
              D = dets:lookup(State#state.ndb, Ng),
              [ Fid || {_Ngram, Fid} <- D ]
          end || Ng <- L ],
    C = lists:sublist(lists:reverse( % top 10
         lists:keysort(2, utils:list_agg(  % aggregate/count dupes
           lists:sort(lists:flatten(R))
         ))
        ),10),
    % now we have a list of candidates, C: [ {'file path atom', Score} .. ]
    % first, gather all the candidate file records
    Files = [ case dets:lookup(State#state.fdb, FileId) of
                [] -> [];
                [P]-> P
              end || {FileId, _CandScore} <- C ],
    % next do the edit-distance calculation to generate a final score:
    Results = [ begin
                    ArtClean = proplists:get_value(artist_clean, FL),
                    TrkClean = proplists:get_value(track_clean, FL),
                    ArtDist = utils:levenshtein(Art, ArtClean),
                    TrkDist = utils:levenshtein(Trk, TrkClean),
                    TLen = length(ArtClean)+length(TrkClean),
                    TDist = utils:min(TLen, ArtDist + TrkDist),
                    Score = (TLen - TDist)/TLen,                    
                    { FL, Score }
                end
                || {_FileId, FL} <- Files ],
    % sort and return to the top N results:
    lists:sublist( lists:reverse( lists:keysort(2,Results) ), 10 ).




