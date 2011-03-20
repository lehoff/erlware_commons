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

%%-----------------------------------------------------------------------------
%% Generators
%%-----------------------------------------------------------------------------

my_dict() ->
    ?SIZED(N,dict(N)).

%% This symbolic generator will create a random instance of a ec_dictionary
%% that will be used in the properties.
dict(0) ->
    {'$call',ec_dictionary,new,[ec_gb_trees]};
dict(N) ->
    frequency([
	       {1, {'$call',ec_dictionary,remove,[integer(),dict(N-1)]}},
	       {2, {'$call',ec_dictionary,add,[integer(),integer(),dict(N-1)]}}
	       ]).


