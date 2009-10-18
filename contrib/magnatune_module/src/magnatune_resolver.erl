% This module acts as a proxy for an instance of the library_dets module
% and loads a special DB of magnatune urls (free for non-commercial use)
-module(magnatune_resolver).

-include("playdar.hrl").
-behaviour(gen_server).
-behaviour(playdar_resolver).

%% API
-export([start_link/0, resolve/3, weight/1, targettime/1, name/1]).
-export([ stats/1, parse/2, check_index/2 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {libpid}).

%%

start_link()            -> gen_server:start_link(?MODULE, [], []).
  
resolve(Pid, Q, Qpid)   -> gen_server:cast(Pid, {resolve, Q, Qpid}).
weight(_Pid)            -> 100.
targettime(_Pid)        -> 20.
name(_Pid)              -> "Magnatune index".

stats(Pid)              -> gen_server:call(Pid, stats).

%% --------------------------------------------------------------------
init([]) ->
	{ok, LibPid} = gen_server:start_link(library_dets, ["magnatune"], []),
	resolver:add_resolver(?MODULE, name(self()), weight(self()), targettime(self()), self()),
	?LOG(info, "Magnatune library pid: ~p",[LibPid]),
	Mpid = self(),
	spawn(fun()->check_index(Mpid, LibPid)end),
    {ok, #state{libpid=LibPid}}.

handle_cast({resolve, Q, Qpid}, State) ->
	library_dets:resolve(State#state.libpid, Q, Qpid),
	{noreply, State}.

handle_call(stats, _From, State) ->
	{reply, library_dets:stats(State#state.libpid), State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

check_index(Mpid, Lpid) ->
	?LOG(info,"Checking if we need to fetch magnatune data..",[]),
	NF = proplists:get_value(num_files, magnatune_resolver:stats(Mpid)),
	if
		NF > 0 -> ok;
		true ->
			% download and import the data
			% it's about 4 meg uncompressed:
			Url = "http://magnatune.com/info/song_info.csv",
			?LOG(info, "Downloading Magnatune data from: ~s",[Url]),
			Ret = http:request(get, {Url, []}, [], [{sync, true}, 
                             						 {stream, "magnatune_data.csv"}, 
                               						 {version, 1.1}]),
			case Ret of
				{ok, saved_to_file} ->
					?LOG(info, "Magnatune data downloaded, processing..",[]),
					parse("magnatune_data.csv",Lpid),
					?LOG(info, "Magnatune data imported ok!", []);
				{error, Reason} ->
					?LOG(warning, "Failed to download magnatune data: ~p",[Reason]),
					error
			end
	end.
	
% For reading song_info.csv from magnatune.com:
parse(File, LibPid) ->
    {ok, Device} = file:open(File, [read]),
	% skip first line:
	io:get_line(Device, ""),
    process_line(Device, 0, LibPid).

process_line(Device, N, LibPid) ->
    case io:get_line(Device, "") of
        eof  -> 
			file:close(Device), {ok, N};
        Line ->
			[Artist, Track, Album, TrkNo, _Year, _Genre, _MagnaGenre, Duration,
			 Url | _Rest] = parse_line(Line),
			P = [
				 {<<"artist">>, 	list_to_binary(Artist)},
				 {<<"album">>,  	list_to_binary(Album)},
				 {<<"track">>,		list_to_binary(Track)},
				 {<<"trackno">>,	list_to_integer(TrkNo)},
				 {<<"bitrate">>,	128},
				 {<<"duration">>,	list_to_integer(Duration)},
				 {<<"mimetype">>,   <<"audio/mpeg">>},
				 {<<"url">>,        list_to_binary(Url)}				
				],
			library_dets:add_file(LibPid, Url, 0, -1, P),
			process_line(Device, N+1, LibPid)
    end.

% csv stuff:
parse_line(Line) -> parse_line(Line, []).

parse_line([], Fields) -> lists:reverse(Fields);
parse_line("," ++ Line, Fields) -> parse_field(Line, Fields);
parse_line(Line, Fields) -> parse_field(Line, Fields).

parse_field("\"" ++ Line, Fields) -> parse_field_q(Line, [], Fields);
parse_field(Line, Fields) -> parse_field(Line, [], Fields).

parse_field("," ++ _ = Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field([C|Line], Buf, Fields) -> parse_field(Line, [C|Buf], Fields);
parse_field([], Buf, Fields) -> parse_line([], [lists:reverse(Buf)|Fields]).

parse_field_q("\"\"" ++ Line, Buf, Fields) -> parse_field_q(Line, [$"|Buf], Fields);
parse_field_q("\"" ++ Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_field_q([C|Line], Buf, Fields) -> parse_field_q(Line, [C|Buf], Fields).
