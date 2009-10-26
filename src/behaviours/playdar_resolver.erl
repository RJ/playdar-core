-module(playdar_resolver).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> 
	[
     {name,1},
     {weight,1},
	 {targettime,1},
	 {resolve,2}
	];

behaviour_info(_Other) ->
    undefined.


