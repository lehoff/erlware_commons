-module(ec_set_set).

-behaviour(ec_set).


%%% API
-export([new/0]).
-export([size/1]).
-export([add_element/2]).
-export([to_list/1]).

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



-spec size(set(_T)) -> non_neg_integer().
size(Set) ->  
    sets:size(Set).

-spec to_list(set(T)) -> list(T).
to_list(Set) ->
    sets:to_list(Set).



		  


		 
