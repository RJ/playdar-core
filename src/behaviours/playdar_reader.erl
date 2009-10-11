-module(playdar_reader).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> 
    [
     {reader_protocols,0}
    ];

behaviour_info(_Other) ->
    undefined.