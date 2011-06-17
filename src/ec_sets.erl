-module(ec_sets).

-behaviour(ec_set).


%%% API
-export([new/0]).
-export([size/1]).
-export([add_element/2]).
-export([del_element/2]).
-export([is_element/2]).
-export([to_list/1]).
-export([from_list/1]).
-export([filter/2]).

-export_type([set/1]).

%% @todo: change to opaque when dialyzer supports it.
-type set(_T) :: set().

-spec new() -> set(_T).
new() ->
    sets:new().

-spec add_element(T,set(T)) -> set(T).			 
add_element(Element,Set) ->
    sets:add_element(Element,Set).

-spec del_element(T,set(T)) -> set(T).			 
del_element(Element,Set) ->
    sets:del_element(Element,Set).

-spec is_element(T,set(T)) -> boolean().
is_element(E,Set) ->
    sets:is_element(E,Set).

-spec size(set(_T)) -> non_neg_integer().
size(Set) ->  
    sets:size(Set).

-spec to_list(set(T)) -> list(T).
to_list(Set) ->
    sets:to_list(Set).

-spec from_list(list(T)) -> set(T).
from_list(L) ->
    sets:from_list(L).
		       
-spec filter(fun((T) -> boolean()), set(T)) -> set(T).
filter(Pred, Set) ->
    sets:filter(Pred,Set).
		  


		 
