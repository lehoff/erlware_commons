%% compile with 
%% erl -pz ebin --make
%% start test with
%%   erl -pz ebin -pz test
%%   proper:module(ec_dictionary_proper).


-module(ec_dictionary_proper).



-include_lib("proper/include/proper.hrl").


%%------------------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------------------

prop_size_increases_with_new_key() ->
    ?FORALL({Dict,K}, {?LET(X,non_neg_integer(), my_dict()),integer()},
	    begin
		Size = ec_dictionary:size(Dict),
		io:format("~p",[Size]),
		case ec_dictionary:has_key(K,Dict) of
		    true ->
			Size == ec_dictionary:size(ec_dictionary:add(K,0,Dict));
		    false ->
			(Size + 1) == ec_dictionary:size(ec_dictionary:add(K,0,Dict))
		end
	    end).

my_dict() ->
    ?SIZED(N,dict(N)).

dict(0) ->
    ec_dictionary:new(ec_gb_trees);
%%    {call,ec_dictionary,new,[ec_gb_trees]};
dict(N) ->
%%    ec_dictionary:add(integer(),integer(),dict(N-1)).
%%    oneof([dict(0),
	   ?LET(D,dict(N-1),
%%		{call,ec_dictionary,add,[integer(),integer(),D]})
		ec_dictionary:add(integer(),integer(),D))
%%	  ])
.
