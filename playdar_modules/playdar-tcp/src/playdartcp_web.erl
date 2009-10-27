-module(playdartcp_web).
-export([http_req/2]).
-include("playdar.hrl").

http_req(Req, DocRoot) ->
    "/playdartcp" ++ Path = Req:get(path), 
    Qs = Req:parse_qs(),
    case proplists:get_value("disconnect", Qs) of
        Name when is_list(Name) ->
            case playdar_auth:consume_formtoken(proplists:get_value("ftok", Qs)) of
                ok ->
                    ?LOG(info, "Disconnecting ~p", [Name]),
                    playdartcp_router:disconnect(Name);
                _ -> 
                    blah
            end,
            Req:respond({302, [{"Location", "/playdartcp"}], <<"">>});
            
        undefined ->
            index(Path, Req, DocRoot)
    end.

index(Path, Req, DocRoot) ->
    case Path of
        "" ->
            Vars = [{ftok, playdar_auth:gen_formtoken()},
                    {peers,
                     [ begin
                           ShareWe   = case WeShare of true -> "yes"; _ -> "no" end,
                           ShareThey = case TheyShare of true -> "yes"; _ -> "no" end,
                           {{{Y,M,D},{H,Min,S}},{ok, Stats}} = playdartcp_conn:stats(P),
                           Date = io_lib:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",[Y,M,D,H,Min,S]),
                           [ {name, N}, 
                             {pid, io_lib:format("~p",[P])},
                             {conndate, Date},
                             {stats, Stats},
                             {weshare, ShareWe},
                             {theyshare, ShareThey} ]
                       end 
                       || {N,P,{WeShare, TheyShare}} <- playdartcp_router:peers() ]
                    }],
            
            playdar_web:render(Req, DocRoot ++ "/playdartcp/index.html", Vars);
        
        _ ->
            Req:not_found()
    end.