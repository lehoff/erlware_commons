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
		case ec_set:is_element(E,Set) of
		    true ->
			Size == ec_set:size(ec_set:add_element(E,Set));
		    false ->
			(Size + 1) == ec_set:size(ec_set:add_element(E,Set))
		end
	    end).

prop_size_decrease_when_removing() ->
    ?FORALL({Set,E}, {set(),element()},
	    begin
		Size = ec_set:size(Set),
		case ec_set:is_element(E,Set) of
		    false ->
			Size == ec_set:size(ec_set:del_element(E,Set));
		    true ->
			(Size - 1) == ec_set:size(ec_set:del_element(E,Set))
		end
	    end).

prop_is_element_after_add_element() ->
    ?FORALL({Set,E}, {set(),element()},
	    begin
		ec_set:is_element(E,ec_set:add_element(E,Set))
	    end).



prop_to_list_after_add_element() ->
    ?FORALL({Set,E},{set(),element()},
	    begin
		Set2 = ec_set:add_element(E,Set),
		lists:usort(ec_set:to_list(Set2)) 
		    == lists:usort([E|ec_set:to_list(Set)])
	    end).

prop_to_list_after_del_element() ->
    ?FORALL({Set,E},{set(),element()},
	    begin
		Set2 = ec_set:del_element(E,Set),
		lists:usort(ec_set:to_list(Set2)) 
		    == lists:usort( lists:delete(E,ec_set:to_list(Set)) )
	    end).



prop_from_list() ->
    ?FORALL({Set,SetType},
	    {set(),set_implementation()},
	    begin
		List = ec_set:to_list(Set),
		S2 = ec_set:from_list(SetType,List),
		List2 = ec_set:to_list(S2),
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
    union([ec_sets]).



