-module(playdar_http_api).
-include("playdar.hrl").
-export([http_req/2, respond/2]).

-define(VER, <<"0.1.1">>).

http_req(Req, DocRoot) ->
    Qs = Req:parse_qs(),
	JsonP = proplists:get_value("jsonp", Qs, ""),
    M = proplists:get_value("method", Qs),
    Auth = playdar_auth:check_auth(proplists:get_value("auth",Qs,"")),
	% If you are using jsonp callbacks, we verify your authcode:
	Authenticated = case JsonP of
						"" -> {true, []};
						_  -> case Auth of
								  A when is_list(A) -> {true, A};
								  undefined -> {false, []}
							  end
					end,
    case M of
        "stat" ->
            case Authenticated of
                {true, _Props} ->
                    R = {struct,[   
                            {"name", <<"playdar">>},
                            {"version", ?VER},
                            {"authenticated", true},
                            {"hostname", <<"TODO">>},
                            {"capabilities", {struct,[
                                {"audioscrobbler", {struct,[ %TODO dynamically
                                    {"plugin", <<"Audioscrobbler">>},
                                    {"description", <<"Scrobbles stuff.">>}
                                ]}}
                            ]}}
                        ]},
                    respond(Req, R);
                {false, _} ->
                    R = {struct, [
                            {"name", <<"playdar">>},
                            {"version", ?VER},
                            {"authenticated", false}
                        ]},
                    respond(Req, R)
            end;
        
        % /comet?id=XX pushes results from queries sent with ?comet=XX
        % doesn't need auth, IDs are opaque.
        "comet" ->
            case proplists:get_value("id", Qs) of
                undefined ->
                    Req:not_found();
                Cid ->
                    playdar_resolver:register_comet(Cid, self()),
                    Jsonp = proplists:get_value("jsonp", Qs),
                    %MType = content_type(Jsonp /= undefined),
                    MType = "multipart/x-mixed-replace",
                    Response = Req:ok({MType, [], chunked}),
                    case Mode = proplists:get_value("mode", Qs) of
                        "raw" -> ok;
                        _ ->
                            Response:write_chunk("<html><head></head><body>")
                    end,
                    comet_loop(Response, Jsonp, Cid, Mode)
            end;
            
        _ ->
			% for all methods other than stat, require auth if jsonp= is used.
			case Authenticated of
				{true, AuthProps} ->
					http_req_authed(Req, DocRoot, M, Qs, AuthProps);
				{false, _} ->
					Req:respond({403, [], <<"<h1>Not Authorised</h1>">>})
			end
    end.
    

http_req_authed(Req, _DocRoot, Method, Qs, _Auth) ->
    case Method of
		% TODO resolve-json, where you just pass the JSON obj instead of many url params.
		% this would be completely generic and work for non-music stuff just the same.
		%
		% Notes on github issue #6
		% ------------------------
		% regarding seeding the resolver with an existing, known result url:
		% i'd like to do this by having the client call resolve and passing the
		% json query object with preexisting results:[..] field, which is just
		% copied verbatim as if a resolver found it.
		
        "resolve" ->
            Artist = proplists:get_value("artist", Qs, ""),
            Album  = proplists:get_value("album", Qs, ""),
            Track  = proplists:get_value("track", Qs, ""),
			Qid    = case proplists:get_value("qid", Qs) of
                		undefined -> playdar_utils:uuid_gen();
                		Str -> list_to_binary(Str)
            		 end,
			Q0 = {struct,[
                    {<<"artist">>, list_to_binary(Artist)}, 
                    {<<"album">>,  list_to_binary(Album)}, 
                    {<<"track">>,  list_to_binary(Track)}
                ]},
            CometPids = case proplists:get_value("comet", Qs) of
                     undefined -> [];
                     Cid -> 
                         case playdar_resolver:get_comet_pid(Cid) of
                             P when is_pid(P) -> [ P ];
                             _ -> []
                         end
            end,
                         
			Q = case proplists:get_value("mimetypes", Qs) of
					undefined -> Q0;
					Strlist ->
						MT = [ list_to_binary(M) || M <- string:tokens(Strlist,",") ],
						{struct, L} = Q0,
						{struct, [{<<"mimetypes">>, MT} | L]}
				end,            
			Qry = #qry{ qid = Qid, obj = Q, local = true },
            Qid = playdar_resolver:dispatch(Qry, [], CometPids),
            R = {struct,[
                    {"qid", Qid}
                ]},
                
            respond(Req, R);
            
        "get_results" ->
            get_results(Req, Qs);
		
		% /api/?method=get_results_long&timeout=4000
		% will return all results found so far in those 4 secs, 
        % and return immediately on solved
		"get_results_long" ->
			get_results_long_poll(Req, Qs);
         
        _ ->
            Req:not_found()
    end.

comet_loop(Resp, Jsonp, Cid, Mode) ->
    receive
        {results, Qid, Results} ->
            R = {struct, [{<<"method">>, <<"results">>},
                          {<<"qid">>, Qid},
                          {<<"results">>, Results}]},
            ?LOG(info, "Writing COMET response for ~s", [Qid]),
            Body = encode_response( R, Jsonp ),
            case Mode of
                "raw" ->
                    Resp:write_chunk( Body ++ "\n\n" );
                _ ->
                    Resp:write_chunk( io_lib:format("<script type=\"text/javascript\">~s</script>\n\n", 
                                            [Body]) )
            end,
            comet_loop(Resp, Jsonp, Cid, Mode)
    end.    

get_results(Req, Qs) ->
    Qid = list_to_binary(proplists:get_value("qid", Qs)),
    case playdar_resolver:results(Qid) of
        undefined ->
            Req:not_found();
        {Results, #qry{obj = Q}, Solved} ->
            R = {struct,[
                         {"qid", Qid},
                         {"poll_interval", 1000},
                         {"poll_limit", 6}, % TODO sum of all targettimes from loaded resolvers
                         {"query", Q},
                         {"solved", Solved},
                         {"results", 
                          [ {struct, proplists:delete(<<"url">>,L)} || 
                            {struct, L} <- Results ]}
                        ]},
            respond(Req, R)
    end.

% long-poll for results, bails after specified time, or when solved->true
get_results_long_poll(Req, Qs) ->
	Qid = list_to_binary(proplists:get_value("qid", Qs)),
    case playdar_resolver:solved(Qid) of
        true ->
            get_results(Req, Qs);
        false ->
            get_results_long_poll_real(Req, Qs)
    end.
        
get_results_long_poll_real(Req, Qs) -> 
    Qid = list_to_binary(proplists:get_value("qid", Qs)),
	Timeout0 = list_to_integer(proplists:get_value("timeout", Qs, "4000")),
    Timeout = erlang:min(Timeout0, 60000),
	case playdar_resolver:register_query_observer(Qid, self()) of
        ok ->
            _ResultsPoll = long_poll_loop(Qid, Timeout, []),
            {Results, #qry{obj = Q}, Solved} = playdar_resolver:results(Qid),
            R = {struct,[
                         {"qid", Qid},
                         {"query", Q},
                         {"solved", Solved},
                         {"results", 
                          [ {struct, proplists:delete(<<"url">>,L)} || 
                            {struct, L} <- Results ]}
                        ]},
            respond(Req, R);
        undefined ->
            ?LOG(error, "LONG POLL on invalid qid: ~s", [Qid]),
            Req:not_found()
    end.

long_poll_loop(Qid, Timeleft, Results) ->
	%?LOG(info, "observer_loop, timeout: ~w", [Timeleft]),
	Start = erlang:now(),
	receive
		{results, Qid, Res} ->
			NewResults = Results ++ Res,
			NewTimeleft = Timeleft - erlang:round(timer:now_diff(erlang:now(), Start)/1000),
			long_poll_loop(Qid, NewTimeleft, NewResults);
		solved ->
            ?LOG(info, "LONG POLL got solved",[]),
			Results
	after Timeleft -> 
			Results
	end.
		
		
% responds with json
respond(Req, R) ->
    Qs = Req:parse_qs(),
    Jsonp = proplists:get_value("jsonp", Qs),
    Ctype = content_type( Jsonp /= undefined ),
    Body  = encode_response(R, Jsonp),
    Req:ok({Ctype, [], Body}).

content_type(true) -> "text/javascript; charset=utf-8";
content_type(false)-> "appplication/json; charset=utf-8". 
    
encode_response(R, Jsonp) ->
    case Jsonp of
        undefined ->
            mochijson2:encode(R);
        F ->
            F++"("++mochijson2:encode(R)++");\n"
    end.
        

    
