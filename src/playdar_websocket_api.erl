-module(playdar_websocket_api).
-include("playdar.hrl").

-export([req/2]).

req(WSReq, _Docroot) ->
    ?LOG(info, "ws api handler fired for ~s", [WSReq:get(path)]),
    Sender = spawn_link(fun()->sender(WSReq)end),
    Data = WSReq:get_data(),

    case (catch mochijson2:decode(Data)) of
        {struct, L} ->
            case proplists:get_value(<<"method">>, L) of
                <<"resolve">> -> resolve(WSReq, L, Sender);
            Err ->
                ?LOG(warning, "Unhandled method in ws call: ~s", [Err]),
                WSReq:send(mochijson2:encode({struct,[{<<"method">>,<<"error">>}, {<<"msg">>,<<"unknown method">>}]})),
                req(WSReq, undefined)
            end;
        _ ->
            ?LOG(warning, "Failed to decode websocket data: ~s", [Data])
    end.

sender(WSReq) ->
    % we'll construct a standard (polling based) results array of all results
    % just so i don't have to change any JS interfaces for now:
    receive 
        Qid ->
            %WSReq:send(M),
            {R, _Qry, _Solved} = playdar_resolver:results(Qid),
            Results = {struct, [{<<"qid">>, Qid}, {<<"method">>,<<"results">>}, {<<"results">>, R}]},
            WSReq:send( mochijson2:encode(Results) ),
            sender(WSReq)
    end.


resolve(WSReq, L, Sender) ->
    Qid = proplists:get_value(<<"qid">>, L),
    Qry = #qry{ qid = Qid, obj = {struct, L}, local = true },
    Cbfun = fun(_StrR) -> 
                %Bin = mochijson2:encode({struct,[{<<"method">>,<<"result">>},{<<"qid">>,Qid}|R]}),
                %?LOG(info, "ws: results callback fired for: '~s'", [Bin]),
                Sender !  Qid
            end,
    ?LOG(info, "ws: resolve",[]),
    Qid = playdar_resolver:dispatch(Qry, [Cbfun]),
    req(WSReq, undefined).

