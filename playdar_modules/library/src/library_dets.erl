-module(library_dets).

-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/2, weight/1, targettime/1, name/1, localonly/1]).
-export([dump_library/1]).
-export([scan/2, stats/1, add_file/5, sync/1 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {scanner, ndb, fdb, customname}).

%%

start_link()            -> gen_server:start_link(?MODULE, [], []).
scan(Pid, Dir)          -> gen_server:call(Pid, {scan, Dir}, infinity).

add_file(Pid, F, Mtime, Size, L) -> gen_server:call(Pid, {add_file, F, Mtime, Size, L}, 60000).
sync(Pid)               -> gen_server:cast(Pid, sync).
  
resolve(Pid, Qry)       -> gen_server:cast(Pid, {resolve, Qry}).
weight(_Pid)            -> 100.
targettime(_Pid)        -> 20.
name(_Pid)              -> "Local Library using DETS".
localonly(_Pid)			-> false.

stats(Pid)              -> gen_server:call(Pid, stats).

dump_library(Pid)       -> gen_server:call(Pid, dump_library, 60000).
%%

% Non-standard init, when used by proxy for another type of library:
init([Name]) when is_list(Name) ->
    DbDir = ?CONFVAL({library,dbdir}, "."),
    {ok, Ndb} = dets:open_file(DbDir++"/"++Name++"_index.db",[{type, bag}]),
    {ok, Fdb} = dets:open_file(DbDir++"/"++Name++".db", [{type, set}]),
    ?LOG(info, "Library index (for ~s) contains ~w files", 
               [Name, proplists:get_value(size, dets:info(Fdb), -1)]),
    {ok, #state{scanner=undefined, ndb=Ndb, fdb=Fdb, customname=Name}};

% Default init, when acting as normal library resolver
init([]) ->
    DbDir = ?CONFVAL({library,dbdir}, "."),
    {ok, Ndb} = dets:open_file(DbDir++"/library_index.db",[{type, bag}]),
    {ok, Fdb} = dets:open_file(DbDir++"/library.db", [{type, set}]),
    ?LOG(info, "Library index contains ~w files", 
               [proplists:get_value(size, dets:info(Fdb), -1)]),
    % start the scanner (kind of a hack, but deadlock if we do it in init here):
    self() ! start_scanner,        
    resolver:add_resolver(?MODULE, self()),
    {ok, #state{scanner=undefined, ndb=Ndb, fdb=Fdb, customname=""}}.

handle_cast(sync, State) -> 
    dets:sync(State#state.fdb),
    dets:sync(State#state.ndb),
    ?LOG(info, "library synced", []),
    {noreply, State};

handle_cast({resolve, #qry{qid = Qid, obj = Obj}}, State) ->
    case Obj of
        {struct, Mq} -> % Mq is a proplist
            Hostname = ?CONFVAL(name, "unknown"),
			Name = case State#state.customname of
					   "" -> Hostname;
					   C  -> Hostname ++ "/" ++ C
				   end,
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
                                {<<"source">>, list_to_binary(Name)}
                            ]},
				resolver:add_results(Qid, Rep)           
            end,
            Art = proplists:get_value(<<"artist">>,Mq,<<"">>),
            Trk = proplists:get_value(<<"track">>,Mq,<<"">>),
            Alb = proplists:get_value(<<"album">>,Mq,<<"">>),
			Mimetypes = proplists:get_value(<<"mimetypes">>,Mq),
            Now = now(),
            Files = search(clean(Art),clean(Alb),clean(Trk),Mimetypes,State),
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
            FileId = list_to_binary(File), %TODO hmm.
			?LOG(library_scan, "~s\t~s\t~s\t~s", [File, Artist, Album, Track]),
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
search(Art,_Alb,Trk,Mimetypes,State) ->
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
	case ?CONFVAL(explain, false) of
		true	-> ?LOG(info, "Scoring: ~s - ~s", [Art, Trk]);
		false	-> ok
	end,
    Results = [ begin
					Scoreit = fun() ->
								ArtClean = proplists:get_value(artist_clean, FL),
								TrkClean = proplists:get_value(track_clean, FL),
								Score = utils:calc_score({ArtClean, Art}, 
														 {TrkClean, Trk}),
								{ FL, Score }
							  end,
					case Mimetypes of
						List when is_list(List) ->
							case lists:member(proplists:get_value(mimetype, FL), Mimetypes) of
								true ->					
									Scoreit();	
								false ->
									{ FL, 0 }
							end;
						_ -> % query doesnt care about mimetype
							Scoreit()
					end
                end
                || {_FileId, FL} <- Files ],
    % sort and return to the top N results:
    lists:sublist( lists:reverse( lists:keysort(2,Results) ), 10 ).

