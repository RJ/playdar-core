-module(playdar_http_api).
-export([http_req/1]).

http_req(Req) ->
    Qs = Req:parse_qs(),
    M = proplists:get_value("method", Qs),
    Auth = playdar_auth:check_auth(proplists:get_value("auth",Qs,"")),
    case M of
        "stat" ->
            case Auth of
                Props when is_list(Props) ->
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
                undefined ->
                    R = {struct, [
                            {"name", <<"playdar">>},
                            {"version", <<"0.1.0">>},
                            {"authenticated", false}
                        ]},
                    respond(Req, R)
            end;

        _ -> http_req_authed(Req, M, Qs, Auth)

    end.
    

http_req_authed(Req, Method, Qs, _Auth) ->
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
                            {"refresh_interval", 1000},
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
        
    
    
