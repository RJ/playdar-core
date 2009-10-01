-module(playdar_resolver).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> 
	[{weight,0},
	 {targettime,0},
	 {resolve,2},
	 {start_link,0}
	];

behaviour_info(_Other) ->
    undefined.


