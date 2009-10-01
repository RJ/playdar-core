
%% This is nowhere near finished and doesn't work.

-module(local).

-behaviour(gen_server).

%% API
-export([start_link/0, start_resolving/1, add_file/2, search/1]).
-export([ngram/1]).
-export([ngram/2]).
-export([list_agg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).

%% API
start_link()        -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_resolving(Q)  -> gen_server:cast(?MODULE, {start_resolving, Q}).

add_file(File, Obj) -> gen_server:call(?MODULE, {add_file, File, Obj}).

search(Term)        -> gen_server:call(?MODULE, {search, Term}).

%% gen_server callbacks
init([]) ->
    dets:open_file(library,[{type, set}]),
    dets:open_file(library_index,[{type, bag}]),
    {ok, #state{}}.

handle_call({search, Term}, _From, State) ->
    A = [ 
            [ File || {{_,_},{_NumOcc,File}} <- dets:lookup(library_index, {artist,Nk}) ]  
            || {Nk, _Nv} <- ngram(Term) 
        ],
    B = list_agg(lists:flatten(A)),
    C = lists:keysort(2, B),
    {reply, C, State};

handle_call({add_file, File, Obj}, _From, State) ->
    dets:insert(library, {File, Obj}),
    Art = proplists:get_value(artist, Obj),
    Trk = proplists:get_value(track,  Obj),
    lists:foreach(fun({K,V})-> dets:insert(library_index, {{artist,K},{V,File}}) end, ngram(Art)),
    lists:foreach(fun({K,V})-> dets:insert(library_index, {{track,K},{V,File}}) end, ngram(Trk)),
    {reply, ok, State}.

handle_cast({start_resolving, _Q}, State) ->
    % search database
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions


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
    

