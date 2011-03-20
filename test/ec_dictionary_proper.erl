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
    ?FORALL({Dict,K}, {my_dict(),integer()},
	    begin
		Size = ec_dictionary:size(Dict),
		case ec_dictionary:has_key(K,Dict) of
		    true ->
			Size == ec_dictionary:size(ec_dictionary:add(K,0,Dict));
		    false ->
			(Size + 1) == ec_dictionary:size(ec_dictionary:add(K,0,Dict))
		end
	    end).

prop_size_decrease_when_removing() ->
    ?FORALL({Dict,K}, {my_dict(),integer()},
	    begin
		Size = ec_dictionary:size(Dict),
		case ec_dictionary:has_key(K,Dict) of
		    false ->
			Size == ec_dictionary:size(ec_dictionary:remove(K,Dict));
		    true ->
			(Size - 1) == ec_dictionary:size(ec_dictionary:remove(K,Dict))
		end
	    end).

prop_get_after_add_returns_correct_value() ->
    ?FORALL({Dict,K,V}, {my_dict(),integer(),integer()},
	    begin
		try ec_dictionary:get(K,ec_dictionary:add(K,V,Dict)) of
		    V ->
			true;
		    _ ->
			false
		catch
		    _:_ ->
			false
		end
	    end).

prop_key_is_present_after_add() ->    
    ?FORALL({Dict,K,V}, {my_dict(),integer(),integer()},
	    begin
		ec_dictionary:has_key(K,ec_dictionary:add(K,V,Dict))
	    end).

prop_value_is_present_after_add() ->    
    ?FORALL({Dict,K,V}, {my_dict(),integer(),integer()},
	    begin
		ec_dictionary:has_value(V,ec_dictionary:add(K,V,Dict))
	    end).

prop_to_list_matches_get() ->
    ?FORALL(SymDict,sym_dict2(),
	    begin
		Dict = eval(SymDict),
		ToList = ec_dictionary:to_list(Dict),
		io:format("ToList:~p~n",[ToList]),
		GetList = 
		    try [ {K,ec_dictionary:get(K,Dict)} || {K,_V} <- ToList ] of
			List -> List
		    catch
			throw:not_found -> key_not_found
		    end,
		io:format("~p == ~p~n",[ToList,GetList]),
		lists:sort(ToList) == lists:sort(GetList)
	    end).


%%-----------------------------------------------------------------------------
%% Generators
%%-----------------------------------------------------------------------------

my_dict() ->
    ?SIZED(N,dict(N)).


dict(0) ->
    ec_dictionary:new(ec_gb_trees);
dict(N) ->
    ?LET(D,dict(N-1),
	 frequency([
		    {1, ec_dictionary:remove(integer(),D)},
		    {2, ec_dictionary:add(integer(),integer(),D)}
		  ])).

sym_dict() ->
    ?SIZED(N,sym_dict(N)).
    
%% This symbolic generator will create a random instance of a ec_dictionary
%% that will be used in the properties.
sym_dict(0) ->
    {'$call',ec_dictionary,new,[ec_gb_trees]};
sym_dict(N) ->
    frequency([
	       {1, {'$call',ec_dictionary,remove,[integer(),dict(N-1)]}},
	       {2, {'$call',ec_dictionary,add,[integer(),integer(),dict(N-1)]}}
	       ]).

sym_dict2() ->
    ?SIZED(N,sym_dict2(N)).

sym_dict2(0) ->
    {call,ec_dictionary,new,[ec_gb_trees]};
sym_dict2(N) ->
    frequency([
	       {1, {call,ec_dictionary,remove,[integer(),dict(N-1)]}},
	       {2, {call,ec_dictionary,add,[integer(),integer(),dict(N-1)]}}
	       ]).
