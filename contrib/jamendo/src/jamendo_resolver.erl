-module(jamendo_resolver).
-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).
 
%% API
-export([start_link/0, resolve/2, weight/1, targettime/1, name/1, localonly/1]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).
 
-record(state, {}).
 
%% API
start_link()            -> gen_server:start_link(?MODULE, [], []).
resolve(Pid, Qry)       -> gen_server:cast(Pid, {resolve, Qry}).
weight(_Pid)            -> 50.
targettime(_Pid)        -> 25.
name(_Pid)              -> "Jamendo Resolver".
localonly(_Pid)		-> false.
 
%% gen_server callbacks
init([]) ->
    playdar_resolver:add_resolver(?MODULE, self()),
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
 
handle_cast({resolve, #qry{obj = Q, qid = Qid}}, State) ->
    ?LOG(info, "asked to resolve: ~s", [Qid]),
    case Q of
        {struct, Mq} -> % Mq is a proplist
            Artist = clean(proplists:get_value(<<"artist">>, Mq, "")),
	    Track = clean(proplists:get_value(<<"track">>, Mq, "")),
	   % Album = clean(proplists:get_value(<<"album">>, Mq, "")),
	    case lookup(Artist,Track) of
		"[]" ->
		    noop;
		Body ->
		    ?LOG(info, "Body is: ~s", [Body]),
		    [{struct,[{_, RetArtist},{_, RetTrack},{_,Duration},{_,RetAlbum},{_,StreamUrl}]}] = mochijson2:decode(Body),
		    Rep =   {struct, [
				      {<<"artist">>, RetArtist},
				      {<<"track">>,  RetTrack},
				      {<<"album">>,  RetAlbum},
				      {<<"duration">>, Duration},
				      {<<"url">>, StreamUrl},
				      {<<"mimetypes">>, ["audio/mpeg"]}
				     ]
			    },
		     ?LOG(info, "sending add results with: ~s", [StreamUrl]),
		     playdar_resolver:add_results(Qid, Rep)
	    end;
        _ -> noop
    end,
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%% Internal functions

clean(Name) when is_binary(Name) ->
    string:to_lower(binary_to_list(Name)).

%% Let the jamendo search query decide which track best matches.
%% This is almost certainly not the best approach. Probably better
%% to get a bunch of results using n=all and rate them ourselves
%% maybe using order parameter to let jamendo help.
%%
%% Not sure how well edoc_lib:escape_uri/1 works.
%% ibrowse_lib:url_encode/1 is a non-OTP alternative.

lookup(Artist,Track) ->
    Art = edoc_lib:escape_uri(Artist),
    Trk = edoc_lib:escape_uri(Track),
    LookupUrl = "http://api.jamendo.com/get2/artist_name+name+duration+album_name+stream/track/json/track_album+album_artist/?artist_name="++Art++"&searchquery="++Trk++"&n=1",
    case http:request(get, {LookupUrl, []}, [], [{sync, true},{version, 1.1}]) of
	{ok, {_, _, Body}} ->
	      Body;
	_ ->
	    []
    end.


