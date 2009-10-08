-module(playdar_reader).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> 
    [
     {init,1}
    ];

behaviour_info(_Other) ->
    undefined.