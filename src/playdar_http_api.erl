-module(playdar_http_api).
-export([http_req/2]).

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
                            {"version", <<"0.1.0">>},
                            {"authenticated", true},
                            {"hostname", <<"TODO">>},
                            {"capabilities", {struct,[]}}
                                %{"local", {struct,[
                                %    {"plugin", <<"Local Library">>},
                                %    {"description", <<"Blah">>}
                                %]}}
                            %]}}
                        ]},
                    respond(Req, R);
                {false, _} ->
                    R = {struct, [
                            {"name", <<"playdar">>},
                            {"version", <<"0.1.0">>},
                            {"authenticated", false}
                        ]},
                    respond(Req, R)
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
        "resolve" ->
            Artist = proplists:get_value("artist", Qs, ""),
            Album  = proplists:get_value("album", Qs, ""),
            Track  = proplists:get_value("track", Qs, ""),
            Qid    = case proplists:get_value("qid", Qs) of
                undefined -> utils:uuid_gen();
                Str -> list_to_binary(Str)
            end,
            Q = {struct,[
                    {<<"artist">>, list_to_binary(Artist)}, 
                    {<<"album">>,  list_to_binary(Album)}, 
                    {<<"track">>,  list_to_binary(Track)}
                ]},
            _Qpid = resolver:dispatch(Q, Qid),
            R = {struct,[
                    {"qid", Qid}
                ]},
                
            respond(Req, R);
            
        "get_results" ->
            Qid = list_to_binary(proplists:get_value("qid", Qs)),
            Qpid = resolver:qid2pid(Qid),
            case Qpid of 
                undefined ->
                    Req:not_found();
                _ ->
                    Results = qry:results(Qpid),
                    Q = qry:q(Qpid),
                    R = {struct,[
                            {"qid", Qid},
                            {"refresh_interval", 1000}, % TODO legacy, to be removed
							{"poll_interval", 1000},
							{"poll_limit", 6}, % TODO sum of all targettimes from loaded resolvers
                            {"query", Q},
                            {"results", 
                                [ {struct, proplists:delete(<<"url">>,L)} || 
                                  {struct, L} <- Results ]}
                        ]},
                    respond(Req, R)
             end;
         
        _ ->
            Req:not_found()
    end.

% responds with json
respond(Req, R) ->
    Qs = Req:parse_qs(),
    case proplists:get_value("jsonp", Qs) of
        undefined ->
            Req:ok({"text/javascript; charset=utf-8", [], mochijson2:encode(R)});
        F ->
            Req:ok({"text/javascript; charset=utf-8", [], 
                    F++"("++mochijson2:encode(R)++");\n"})
    end.
        
    
    
