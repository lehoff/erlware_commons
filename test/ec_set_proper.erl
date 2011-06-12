%% compile with
%% erl -pz ebin --make
%% start test with
%%   erl -pz ebin -pz test
%%   proper:module(ec_set_proper).


-module(ec_set_proper).

-compile(export_all).

-include_lib("proper/include/proper.hrl").


%%------------------------------------------------------------------------------
%% Properties
%%------------------------------------------------------------------------------

prop_size_increases_with_new_element() ->
    ?FORALL({Set,E}, {set(),element()},
	    begin
		Size = ec_set:size(Set),
		case ec_set:has_key(E,Set) of
		    true ->
			Size == ec_set:size(ec_set:add(E,Set));
		    false ->
			(Size + 1) == ec_set:size(ec_set:add(E,Set))
		end
	    end).

prop_size_decrease_when_removing() ->
    ?FORALL({Dict,K}, {set(),integer()},
	    begin
		Size = ec_set:size(Dict),
		case ec_set:has_key(K,Dict) of
		    false ->
			Size == ec_set:size(ec_set:remove(K,Dict));
		    true ->
			(Size - 1) == ec_set:size(ec_set:remove(K,Dict))
		end
	    end).

prop_get_after_add_returns_correct_value() ->
    ?FORALL({Dict,K,V}, {set(),key(),value()},
	    begin
		try ec_set:get(K,ec_set:add(K,V,Dict)) of
		    V ->
			true;
		    _ ->
			false
		catch
		    _:_ ->
			false
		end
	    end).

prop_add_does_not_change_values_for_other_keys() ->
    ?FORALL({Dict,K,V},  {set(),key(),value()},
	    begin
		Keys = ec_set:keys(Dict),
		?IMPLIES(not lists:member(K,Keys),
			 begin
			     Dict2 = ec_set:add(K,V,Dict),
			     try lists:all(fun(B) -> B end,
					   [ ec_set:get(Ka,Dict) ==
						 ec_set:get(Ka,Dict2) ||
					       Ka <- Keys ]) of
				 Bool -> Bool
			     catch
				 throw:not_found -> true
			     end
			 end)
	    end).



prop_key_is_present_after_add() ->
    ?FORALL({Dict,K,V}, {set(),integer(),integer()},
	    begin
		ec_set:has_key(K,ec_set:add(K,V,Dict))	    end).

prop_value_is_present_after_add() ->
    ?FORALL({Dict,K,V}, {set(),integer(),integer()},
	    begin
		ec_set:has_value(V,ec_set:add(K,V,Dict))
	    end).

prop_to_list_matches_get() ->
    ?FORALL(Dict,set(),
	    begin
		%% Dict = eval(SymDict),
		%% io:format("SymDict: ~p~n",[proper_symb:symbolic_seq(SymDict)]),
		ToList = ec_set:to_list(Dict),
		%% io:format("ToList:~p~n",[ToList]),
		GetList =
		    try [ {K,ec_set:get(K,Dict)} || {K,_V} <- ToList ] of
			List -> List
		    catch
			throw:not_found -> key_not_found
		    end,
		%% io:format("~p == ~p~n",[ToList,GetList]),
		lists:sort(ToList) == lists:sort(GetList)
	    end).

prop_value_changes_after_update() ->
    ?FORALL({Dict, K1, V1, V2},
	    {set(),
	     key(), value(), value()},
	    begin
		Dict1 = ec_set:add(K1, V1, Dict),
		Dict2 = ec_set:add(K1, V2, Dict1),
		V1 == ec_set:get(K1, Dict1) andalso
		    V2 == ec_set:get(K1, Dict2)
	    end).

prop_remove_removes_only_one_key() ->
    ?FORALL({Dict,K},
	    {set(),key()},
	    begin
		{KeyGone,Dict2} = case ec_set:has_key(K,Dict) of
				      true ->
					  D2 = ec_set:remove(K,Dict),
					  {ec_set:has_key(K,D2) == false,
					   D2};
				      false ->
					  {true,ec_set:remove(K,Dict)}
				  end,
		OtherEntries = [ KV || {K1,_} = KV <- ec_set:to_list(Dict),
				       K1 /= K ],
		KeyGone andalso 
		    lists:sort(OtherEntries) == lists:sort(ec_set:to_list(Dict2))
	    end).

prop_from_list() ->
    ?FORALL({Dict,DictType},
	    {set(),set_implementation()},
	    begin
		List = ec_set:to_list(Dict),
		D2 = ec_set:from_list(DictType,List),
		List2 = ec_set:to_list(D2),
		lists:sort(List) == lists:sort(List2)
	    end).
	    

%%-----------------------------------------------------------------------------
%% Generators
%%-----------------------------------------------------------------------------

element() -> union([integer(),atom()]).

set() ->
    ?SIZED(N,set(N)).

%% This symbolic generator will create a random instance of a ec_set
%% that will be used in the properties.
set(0) ->
    ?LET(Set,set_implementation(),
	 {'$call',ec_set,new,[Set]});
set(N) ->
    ?LAZY(
       frequency([
		  {1, set(0)},
		  {3, {'$call',ec_set,del_element,[element(),set(N-1)]}},
		  {6, {'$call',ec_set,add_element,[element(),set(N-1)]}}
		 ])
      ).

set_implementation() ->
    union([ec_set_set]).



