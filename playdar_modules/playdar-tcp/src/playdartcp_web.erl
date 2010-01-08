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
            Streams = get_streams(),
            ?LOG(info, "~p", [Streams]),
            Vars = [{ftok, playdar_auth:gen_formtoken()},
                    {streams, Streams},
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

get_streams() ->
    S  = playdartcp_router:streams(),
%    ?LOG(info, "Streams: ~p", [S]),
    S2 = [ begin
              Sock = proplists:get_value(sock, L),
              Current =  proplists:get_value(mode, L), 
              case inet:getstat(Sock) of
                   {ok,BW} when is_list(BW) ->
                       % elapsed transfer time in secs:
                       Tdiff = timer:now_diff(erlang:now(),  proplists:get_value(now, L))/1000000,

                       case Current of
                           receive_stream ->
                               B = proplists:get_value(recv_oct, BW),
                               A = ((B*8)/Tdiff)/1024, % kbps
                               [{transferred, B},
                                {kbps, erlang:round(A)}];
                           send_stream ->
                               B = proplists:get_value(send_oct, BW),
                               A = ((B*8)/Tdiff)/1024, % kbps
                               [{transferred, B},
                                {kbps, erlang:round(A)}]
                       end;
                {error,_} ->
                    [{transferred, -1},{kbps,-1}]
              end
              ++
              [ {mode, atom_to_list(Current)},
                {sid, Sid} ] 
              ++
              case proplists:get_value(track, L) of
                  undefined -> [{artist,""},{album,""},{track,""}];
                  {struct, Trk} -> 
                         [ {artist,proplists:get_value(<<"artist">>,Trk,"")},
                           {track, proplists:get_value(<<"track">>,Trk,"")},
                           {album, proplists:get_value(<<"album">>,Trk,"")}
                         ]
              end                      
           end  
           || {_Pid, Sid, L} <- S ],
    S2.
    
    