-module(p2p_web).
-export([http_req/2]).
-include("playdar.hrl").

http_req(Req, DocRoot) ->
    %Qs = Req:parse_qs(),
    "/p2p" ++ Path = Req:get(path), 
    case Path of
        "" ->
            Vars = [{peers,
             [ begin
                   {{{Y,M,D},{H,Min,S}},{ok, Stats}} = p2p_conn:stats(P),
                   Date = io_lib:format("~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",[Y,M,D,H,Min,S]),
                   [ {name, N}, 
                     {pid, io_lib:format("~p",[P])},
                     {conndate, Date},
                     {stats, Stats} ]
               end 
               || {N,P} <- p2p_router:peers() ]
            }],
            
            playdar_web:render(Req, DocRoot ++ "/p2p/index.html", Vars);
        
        _ ->
            Req:not_found()
    end.